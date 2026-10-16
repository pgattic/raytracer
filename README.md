# Haskell Ray Tracer

Sphere and triangle intersections, Phong lighting, glossy reflections, rough
refraction, area lights with soft shadows, jittered antialiasing, and BVH acceleration.

## Run

```sh
nix develop -c cabal run raytracer
nix develop -c cabal test all
```

The default run writes preview versions of scene1.ppm and scene2.ppm at 640x360,
with 16 samples per pixel, four samples per area light, and four secondary-ray
bounces. Higher settings can be expensive because recursive hits also sample lights.

For the higher-quality demonstration renders:

```sh
nix develop -c cabal run raytracer -- --height 360 --samples 32 --shadow-samples 4 --depth 5 --seed 42
```

Use --height 720 for 1280x720 output.

For a quick draft:

```sh
nix develop -c cabal run raytracer -- --scene 1 --height 180 --samples 8 --shadow-samples 4 --depth 4 --output /tmp/draft.ppm
```

Use --scene 1, 2, or both; --height and --samples must be positive;
--depth can be zero. --output requires a single scene.
Each scene has its own composed camera. --eye X Y Z and --target X Y Z override
the camera position and look-at point independently.
The up vector is positive Y; viewing directly along that vector is rejected.
Run with --help for the option summary.

--shadow-samples controls samples per area light at each intersection and must
be positive. --seed selects an unsigned 64-bit seed (default 42). The same seed
and settings reproduce the same image. Different pixels, light samples, and
recursive paths receive separate random streams. More pixel samples reduce noise
in glossy reflections and rough transmission as well as antialiasing.

## Demonstration Scenes

Both scenes use BVH traversal, stochastic pixel samples, rectangular area lights,
and floors made from triangles.

- Scene 1, Sphere Garden: a low camera views a seeded field of small colored
  spheres surrounding large clear-glass, chrome, and warm matte spheres.
  Smaller frosted-glass and glossy-metal accents demonstrate rough transmission
  and reflection. The arrangement is inspired by the supplied sphere-demo JPG.
- Scene 2, Glass Gallery: clear and frosted glass sit side by side in front of
  colored sphere stacks. Polished and brushed metal sit behind them, making
  sharp versus blurred reflection easy to compare.

The spheres sit on the floor or in touching stacks, with clearance around the
large objects to avoid intersections. Most small spheres and the floors are
matte, keeping the reflected colors readable and avoiding unnecessary recursion.
Both scenes use a large area light for soft shadows and a dim directional fill.
The renderer does not yet implement the reference image's depth of field.

Material reflectionRoughness and transmissionRoughness range from 0 (sharp)
to 1 (a hemisphere-wide cone). Rays are jittered around the ideal reflection
or refraction direction and constrained to the appropriate surface hemisphere.
Total internal reflection uses the reflection roughness.

AreaLight takes a corner, two edge vectors spanning its rectangle, and a color.
Its samples are averaged, so increasing the sample count does not brighten it.
Like the existing point-light model, this is an artistic illumination model:
it has no inverse-square falloff, emitter cosine factor, or visible emissive mesh.

## Code

- app/Scenes.hs contains submission scenes, geometry helpers, and material presets.
- app/Main.hs parses options and writes images.
- src/Renderer.hs handles lighting and recursive rays.
- src/Optics.hs contains reflection, refraction, and secondary-ray offsets.
- src/Sampling.hs generates seeded jittered strata and cone directions.
- src/Bounds.hs and src/Objects/Object.hs implement bounding boxes and BVH traversal.

Colors stay linear during rendering and averaging; PPM conversion applies
gamma-2 encoding. rayBounds clips primary camera rays in ray-parameter units,
not necessarily world distance. Secondary rays have their own positive,
unbounded interval.

Refraction assumes isolated objects surrounded by air. Nested dielectric media
are not tracked. Transparent shadows approximate straight-line transmission
through each surface; they do not reproduce refractive caustics.
Keep reflectivity and transparency in [0,1] with a sum no greater than one,
and use a positive refractiveIndex. Camera dimensions must be positive.
