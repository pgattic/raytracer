# Haskell Ray Tracer

Sphere and triangle intersections, Phong lighting, reflections, refraction,
transparent shadows, deterministic supersampling, and BVH acceleration.

## Run

```sh
nix develop -c cabal run raytracer
nix develop -c cabal test all
```

The default run writes scene1.ppm and scene2.ppm at 1280x720, with nine samples
per pixel and six secondary-ray bounces.

For a quick draft:

```sh
nix develop -c cabal run raytracer -- --scene 1 --height 180 --samples 2 --depth 4 --output /tmp/draft.ppm
```

Use --scene 1, 2, or both; --height and --samples must be positive;
--depth can be zero. --output requires a single scene.
--eye X Y Z and --target X Y Z control the camera position and look-at point.
The up vector is positive Y; viewing directly along that vector is rejected.
Run with --help for the option summary.

## Code

- app/Scenes.hs contains submission scenes, geometry helpers, and material presets.
- app/Main.hs parses options and writes images.
- src/Renderer.hs handles lighting and recursive rays.
- src/Optics.hs contains reflection, refraction, and secondary-ray offsets.
- src/Sampling.hs generates deterministic samples covering both pixel axes.
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
