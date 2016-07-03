#include <cassert>
#include <cmath>
#include <experimental/optional>
#include <functional>
#include <algorithm>
#include <random>
#include <SDL.h>
#include <SDL_timer.h>
#include <vector>
#include <thread>
#include <iostream>

#define val const auto
#define mut auto

#define optional std::experimental::optional
constexpr float EPSILON = 0.0001f;
constexpr int REUSE_EYE_RAY_TIMES = 20;
constexpr int MAX_BOUNCES = 4;

struct Color {
    float r,g,b;
    Color(): r{0.0f}, g{0.0f}, b{0.0f} {}
    explicit Color(float b): r{b}, g{b}, b{b} {} 
    Color(float r, float g, float b): r{r}, g{g}, b{b} {}
    Uint32 pixelVal() const {
        return static_cast<Uint32>(255*b) +
              (static_cast<Uint32>(255*g) << 8) +
              (static_cast<Uint32>(255*r) << 16);
    }
    Color &operator+=(const Color &o) { r+=o.r; g+=o.g; b+=o.b; return *this; }
    friend Color operator*(const float f, const Color &c) { return Color {f*c.r, f*c.g, f*c.b}; }
    friend Color operator*(const Color &a, const Color &b) { return Color {a.r*b.r, a.g*b.g, a.b*b.b}; }
    friend Color operator+(const Color &a, const Color &b) { return { a.r+b.r, a.g+b.g, a.b+b.b }; }
    friend bool operator<(const Color &a, const Color &b) {
        return std::max({a.r, a.g, a.b}) < std::max({b.r, b.g, b.b});
    }
    friend std::ostream &operator<<(std::ostream &os, const Color &c) {
        os << "Color{" << c.r << "," << c.g << "," << c.b << "}\n";
        return os;
    }
};

struct Vec3 {
    float x, y, z;
    Vec3(): x{0}, y{0}, z{0} {}
    Vec3(float x, float y, float z): x{x}, y{y}, z{z} {}
    friend Vec3 operator*(const Vec3 &v, const float f) { return { f*v.x, f*v.y, f*v.z }; }
    friend Vec3 operator*(const float f, const Vec3 &v) { return v*f; }
    friend Vec3 operator*(const Vec3 &a, const Vec3 &b) { return { a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x }; }
    friend Vec3 operator+(const Vec3 &a, const Vec3 &b) { return { a.x+b.x, a.y+b.y, a.z+b.z }; }
    friend Vec3 operator-(const Vec3 &a, const Vec3 &b) { return { a.x-b.x, a.y-b.y, a.z-b.z }; }
    friend Vec3 operator-(const Vec3 &a) { return { -a.x, -a.y, -a.z }; }
    friend float dot(const Vec3 &a, const Vec3 &b) { return a.x*b.x + a.y*b.y + a.z*b.z; }
    friend Vec3 normalized(const Vec3 &v) {
        const float len = std::sqrt(dot(v, v));
        return (1.0f / len) * v;
    }
    friend std::ostream &operator<<(std::ostream &os, const Vec3 &v) {
        os << "Vec3{" << v.x << "," << v.y << "," << v.z << "} ";
        return os;
    }
};

struct Ray {
    Vec3 origin;
    Vec3 dir;
    Ray(): origin{}, dir{} {}
    Ray(Vec3 origin, Vec3 dir): origin{origin}, dir{dir} {}
};

struct Primitive {
    struct _Sphere { Vec3 origin; float radius; };
    struct _Triangle { Vec3 v1, v2, v3; };
    union { _Sphere sphere; _Triangle tri; };
    enum Type { Sphere, Triangle } type;

    friend std::ostream &operator<<(std::ostream &os, const Primitive &prim) {
        if (prim.type == Sphere) {
            os << "Sphere(" << &prim << ": " << prim.sphere.origin << ", radius=" << prim.sphere.radius << std::endl;
        } else if (prim.type == Triangle) {
            os << "Triangle(" << &prim << ": " << prim.tri.v1 << "," << prim.tri.v2 << "," << prim.tri.v3 << std::endl;
        } else {
            abort();
        }
        return os;
    }
};

Primitive Sphere(Vec3 origin, float radius) {
    Primitive p {};
    p.type = Primitive::Sphere;
    new(&p.sphere) Primitive::_Sphere{origin, radius};
    return p;
}

Primitive Triangle(const Vec3 &v1, const Vec3 &v2, const Vec3 &v3) {
    Primitive p {};
    p.type = Primitive::Triangle;
    new(&p.tri) Primitive::_Triangle{v1,v2,v3};
    return p;
}

struct Material {
    Color emissive, diffuse;

    friend std::ostream &operator<<(std::ostream &os, const Material &mat) {
        os << "Material {emissive=" << mat.emissive << ", diffuse=" << mat.diffuse << "}\n";
        return os;
    }
};

struct SceneObj {
    Primitive prim;
    Material mat;

    friend std::ostream &operator<<(std::ostream &os, const SceneObj &isect) {
        os << isect.prim << isect.mat; return os;
    }
};

struct RayIntersection {
    float dist;
    const SceneObj *obj;
    Ray ray;

    friend std::ostream &operator<<(std::ostream &os, const RayIntersection &isect) {
        //os << "Dist: " << isect.dist << " " << isect.obj;
        os  << "Dist: " << isect.dist << " org " << isect.ray.origin << " dir " << 
            isect.ray.dir << std::endl;
        return os;
    }
};

struct Path {
    int numBounces;
    std::array<RayIntersection, MAX_BOUNCES> isects;

    Path(): numBounces{0} {}

    friend std::ostream &operator<<(std::ostream &os, const Path &p) {
        os << "Remaining bounces: " << p.numBounces << std::endl;
        for (mut i = begin(p.isects); i!=end(p.isects); ++i) {
        //for (val i : p.isects) {
            os << "isect " << (RayIntersection*)&(*i) << ":" << *i << std::endl;
        }
        return os;
    }
};

const std::vector<SceneObj> scene = {
    {
        Sphere(Vec3 {0,-1.5,-4}, 1),
        Material { Color(0,0,0), Color(1,1,1) }
    }, {
        Sphere(Vec3 {2, -1, -4}, 0.5),
        Material { Color{0,1,0}, Color{1,1,1} }
    }, {
        Sphere(Vec3 {-2,-1,-4}, 0.5),
        Material { Color{1,0,0}, Color{1,1,1} }
    }, {
        Sphere(Vec3 {0,0,-4}, 0.5),
        Material { Color{0,0,1}, Color{1,1,1} }
    },
    // floor
    {
        Triangle(Vec3{-100,-2,0}, Vec3{-100,-2,-100}, Vec3{100,-2,0}),
        Material {Color{0,0,0}, Color{1,1,1}}
    }, {
        Triangle(Vec3{100,-2,0}, Vec3{-100,-2,-100}, Vec3{100,-2,-100}),
        Material {Color{0,0,0}, Color{1,1,1}}
    },
    // back wall
    {
        Triangle(Vec3{-100,-2,-10}, Vec3{100,100,-10}, Vec3{100,-2,-10}),
        Material{Color{}, Color{1,1,1}}
    }, {
        Triangle(Vec3{100,100,-20},Vec3{-100,100,-10},Vec3{-100,-2,-10}),
        Material{Color{}, Color{1,1,1}}
    },
    // light
    {
        Sphere(Vec3{0,8,-4}, 3),
        Material{Color{1,1,.8}, Color{1}}
    }
};

using rngFn = std::function<float()>;

void _exit() { SDL_Quit(); exit(0); }

void eventLoop()
{
    SDL_Event event;

    while (SDL_PollEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT: _exit();
            default: break;
        }
    }
}

auto rayPrimitiveIntersects(const Ray &ray, const SceneObj *obj) -> optional<RayIntersection>
{
    switch (obj->prim.type) {
        case Primitive::Triangle: {
            val a = obj->prim.tri.v1;
            val b = obj->prim.tri.v2;
            val c = obj->prim.tri.v3;
            val n = (c - a) * (b - a);
            val v0_cross = (b - ray.origin) * (a - ray.origin);
            val v1_cross = (a - ray.origin) * (c - ray.origin);
            val v2_cross = (c - ray.origin) * (b - ray.origin);
            val nominator = dot(n, a - ray.origin);
            val v0d = dot(v0_cross, ray.dir);
            val v1d = dot(v1_cross, ray.dir);
            val v2d = dot(v2_cross, ray.dir);
            if ( ((v0d > 0.0) && (v1d > 0.0) && (v2d > 0.0)) || 
                 ((v0d < 0.0) && (v1d < 0.0) && (v2d < 0.0))) {
                val dist = nominator / (dot(ray.dir, n));
                if (dist > EPSILON) {
                    return optional<RayIntersection>(RayIntersection{dist, obj, ray});
                }
            }
            return optional<RayIntersection>();
        }
        case Primitive::Sphere: {
            val v = ray.origin - obj->prim.sphere.origin;
            val b = -(dot(v, ray.dir));
            val sq_det = (b*b) - dot(v, v) + obj->prim.sphere.radius*obj->prim.sphere.radius;
            if (sq_det > 0.0f) {
                val det = std::sqrt(sq_det);
                val i1 = b - det;
                val i2 = b + det;
                if (i2 > 0) {
                    if (i1 < 0) return optional<RayIntersection>(RayIntersection{i2, obj, ray});
                    else return optional<RayIntersection>(RayIntersection{i1, obj, ray});
                }
            }
            return optional<RayIntersection>();
        }
        default:
            assert(0);
    }
}

auto findFirstIntersection(const Ray &ray, const std::vector<SceneObj> &scene) -> optional<RayIntersection> 
{
    mut nearest = optional<RayIntersection>();

    for (mut i=0U; i<scene.size(); ++i) {
        auto isect = rayPrimitiveIntersects(ray, &scene[i]);

        if (isect) {
            if (nearest) {
                if (isect.value().dist < nearest.value().dist) {
                    nearest.swap(isect);
                }
            } else {
                nearest.swap(isect);
            }
        }
    }
    return nearest;
}

Vec3 isectPos(const RayIntersection &isect)
{
    assert(isect.dist >= 0);
    return isect.ray.origin + (isect.dist * isect.ray.dir);
}

Vec3 isectNormal(const RayIntersection &isect)
{
    if (isect.obj->prim.type == Primitive::Sphere) {
        return normalized(isectPos(isect) - isect.obj->prim.sphere.origin);

    } else if (isect.obj->prim.type == Primitive::Triangle) {
        val &v1 = isect.obj->prim.tri.v1;
        val &v2 = isect.obj->prim.tri.v2;
        val &v3 = isect.obj->prim.tri.v3;
        return normalized((v2-v1) * (v2-v3));
    } else {
        abort();
    }
}

Vec3 flipVectorToHemisphere(const Vec3 &flipee, const Vec3 &norm)
{
    if (dot(flipee, norm) > 0.0f) {
        return flipee;
    } else {
        return -flipee;
    }
}

Vec3 randomVectorInHemisphere(const Vec3 &norm, rngFn &dice)
{
    return normalized(
        flipVectorToHemisphere(
            Vec3 { 0.5f - dice(), 0.5f - dice(), 0.5f - dice() },
            norm
        )
    );
}

Ray newRandomRayFromIsect(const RayIntersection &isect, rngFn &dice)
{
    val lastIsectNorm = isectNormal(isect);
    val rayStartPos = isectPos(isect) + (EPSILON * lastIsectNorm);
    val randDir = randomVectorInHemisphere(lastIsectNorm, dice);
    return Ray { rayStartPos, randDir };
}

void makeRayScatterPath(const Ray& ray, const std::vector<SceneObj> &scene, rngFn &dice, Path &path)
{
    val isect = findFirstIntersection(ray, scene);

    if (isect) {
        path.isects[path.numBounces++] = *isect;

        if (path.numBounces < MAX_BOUNCES) {
            val next_ray = newRandomRayFromIsect(*isect, dice);
            makeRayScatterPath(next_ray,
                               scene,
                               dice,
                               path);
        }
    }
}

Color collectLightFromPath(const Path &path)
{
    mut color = Color {0.0f, 0.0f, 0.0f};

    for (mut i=path.numBounces-1; i>=0; --i) {
        val surface_normal = isectNormal(path.isects[i]);
        val cos_theta = dot(-(normalized(path.isects[i].ray.dir)), surface_normal);
        val reflected = cos_theta * (color * path.isects[i].obj->mat.diffuse);

        color = path.isects[i].obj->mat.emissive + reflected;
    }
    return color;
}

void pathTraceRays(const std::vector<Ray> rays, const std::vector<SceneObj> &scene, rngFn &dice, Color *photonBuffer)
{
    mut photonBucket = photonBuffer;
    mut rayIter = begin(rays);

    for (; rayIter != end(rays); ++rayIter, ++photonBucket) {
        // path trace from this eye ray a number of times, in order to
        // make repeated use of calculated eye ray and hot scene cache
        for (mut i=0; i<REUSE_EYE_RAY_TIMES; i++) {
            Path path;
            makeRayScatterPath(*rayIter, scene, dice, path);
            *photonBucket += collectLightFromPath(path);
        }
    }
}

auto eyeRays(const int width, const int height, const int startY, const int endY, const float subPixX, const float subPixY) -> std::vector<Ray>
{
    val fw = static_cast<float>(width);
    val fh = static_cast<float>(height);
    val aspect = fw / fh;
    val topLeft_ = Vec3 { -aspect, 1.0f, -1.0f };
    val rightStep = (1.0f / (fw-1)) * Vec3 { 2*aspect, 0, 0 };
    val downStep = (1.0f / (fh-1)) * Vec3 { 0, -2, 0 };
    val topLeft = topLeft_ + subPixX * rightStep + subPixY * downStep;

    std::vector<Ray> rays(width*(1+endY-startY));
    for (mut y=startY; y<=endY; y++) {
        for (mut x=0; x<width; x++) {
            rays[x + width*(y-startY)] = Ray {
                Vec3 {0,0,0},
                normalized(topLeft + (x * rightStep) + (y * downStep))
            };
        }
    }
    return rays;
}

void pathTraceScene(const std::vector<SceneObj> &scene, const int width, const int height, const int startY, const int endY,
                    Color *photonBuffer, rngFn &dice)
{
    const float subPixX = dice();
    const float subPixY = dice();

    pathTraceRays(eyeRays(width, height, startY, endY, subPixX, subPixY), scene, dice, photonBuffer);
}

void renderPixels(SDL_Surface *screen, const std::vector<Color> &photonBuffer, std::function<const Color(Color)> colorTransformFn)
{
    for (int y=0; y<screen->h; y++) {
        for (int x=0; x<screen->w; x++) {
            val c = colorTransformFn(photonBuffer[x + screen->w*y]);
            SDL_Rect dest = { (Sint16)x, (Sint16)y, 1, 1 };
            SDL_FillRect(screen, &dest, c.pixelVal());
        }
    }
}

void hdrPostprocessBlit(SDL_Surface *screen, std::vector<Color> &photonBuffer)
{
    val maxColor = *std::max_element(cbegin(photonBuffer), cend(photonBuffer), [] (auto a, auto b) { return a<b;});
    const float brightness = 1.0f / sqrt(std::max({maxColor.r, maxColor.g, maxColor.b}));

    renderPixels(screen, photonBuffer, [brightness] (const Color c) -> Color {
        return Color(brightness * sqrt(c.r), brightness*sqrt(c.g), brightness*sqrt(c.b));
    });
}

void mainLoop(SDL_Surface *screen)
{
    std::random_device r;
    std::default_random_engine generator(r());
    std::uniform_real_distribution<float> distribution {0.0, 1.0};
    rngFn dice1 = std::bind(distribution, generator);
    rngFn dice2 = std::bind(distribution, generator);
    rngFn dice3 = std::bind(distribution, generator);
    rngFn dice4 = std::bind(distribution, generator);

    std::vector<Color> photonBuffer = std::vector<Color>(screen->w * screen->h);

    for (;;) {
        eventLoop();
        val t = SDL_GetTicks();

        std::thread t1([&] {
            pathTraceScene(scene, screen->w, screen->h, 0, screen->h/4 - 1, &photonBuffer[0], dice1);
        });
        std::thread t2([&] {
            pathTraceScene(scene, screen->w, screen->h, screen->h/4, screen->h/2 - 1, &photonBuffer[(screen->h/4)*screen->w], dice2);
        });
        std::thread t3([&] {
            pathTraceScene(scene, screen->w, screen->h, screen->h/2, 2*screen->h/3 - 1, &photonBuffer[(screen->h/2)*screen->w], dice3);
        });
        pathTraceScene(scene, screen->w, screen->h, 2*screen->h/3, screen->h-1, &photonBuffer[(2*screen->h/3)*screen->w], dice4);

        t1.join();
        t2.join();
        t3.join();

        hdrPostprocessBlit(screen, photonBuffer);

        val t_ = SDL_GetTicks();
        std::cout << (t_ - t) << " ms per frame, "
            << ((1000LL * screen->w * screen->h * REUSE_EYE_RAY_TIMES) / (Sint64)(t_ - t)) << " paths per second\n";
        SDL_Flip(screen);
        SDL_Delay(100);
    }
}

int main()
{
    SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER);
    auto *screen = SDL_SetVideoMode(512, 512, 32, SDL_SWSURFACE);
    mainLoop(screen);
}
