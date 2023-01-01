open System
open System.Numerics

let maxDepth = 5

type Color = Vector3

let white = Color.One

let grey = Color(0.5f)

let black = Color.Zero

let background = black

let defaultColor = black

type Camera =
    { forward: Vector3
      right: Vector3
      up: Vector3
      pos: Vector3 }

let mkCamera (pos: Vector3) (lookAt: Vector3) =
    let down = -Vector3.UnitY
    let forward = Vector3.Normalize(lookAt - pos)
    let right = 1.5f * Vector3.Normalize(Vector3.Cross(forward, down))
    let up = 1.5f * Vector3.Normalize(Vector3.Cross(forward, right))

    { forward = forward
      right = right
      up = up
      pos = pos }

type Ray = { start: Vector3; direction: Vector3 }

type Surface =
    { diffuse: Color -> Color
      specular: Color -> Color
      reflect: Color -> float32
      roughness: float32 }

type Sphere =
    { center: Vector3
      radius: float32
      surface: Surface }

type Plane =
    { normal: Vector3
      offset: float32
      surface: Surface }

type Intersection =
    { thing: Thing
      ray: Ray
      distance: float32 }

and Thing =
    | Sphere of Sphere
    | Plane of Plane

type Light = { position: Vector3; color: Color }

type Scene =
    { things: Thing list
      lights: Light list
      camera: Camera }

let surface (thing: Thing) =
    match thing with
    | Sphere sphere -> sphere.surface
    | Plane plane -> plane.surface

let normal (thing: Thing) (pos: Vector3) =
    match thing with
    | Sphere sphere -> Vector3.Normalize(pos - sphere.center)
    | Plane plane -> plane.normal

let intersect (thing: Thing) (ray: Ray) =
    match thing with
    | Sphere sphere ->
        let eo = sphere.center - ray.start
        let v = Vector3.Dot(eo, ray.direction)
        let mutable dist = 0f

        if v >= 0f then
            let disc = sphere.radius ** 2f - (Vector3.Dot(eo, eo) - v ** 2f)

            if disc >= 0f then
                dist <- v - MathF.Sqrt(disc)

        match dist with
        | dist when dist = 0f -> None
        | _ ->
            Some
                { thing = thing
                  ray = ray
                  distance = dist }
    | Plane plane ->
        let denom = Vector3.Dot(plane.normal, ray.direction)

        match denom with
        | denom when denom > 0f -> None
        | _ ->
            let dist = (Vector3.Dot(plane.normal, ray.start) + plane.offset) / -denom

            Some
                { thing = thing
                  ray = ray
                  distance = dist }

let surfaces =
    {| shiny =
        { diffuse = fun _ -> white
          specular = fun _ -> Color(0.5f)
          reflect = (fun _ -> 0.7f)
          roughness = 250f }
       checkerboard =
        { diffuse =
            fun pos ->
                if (MathF.Floor(pos.Z) + MathF.Floor(pos.X)) % 2f <> 0f then
                    white
                else
                    black
          specular = fun _ -> white
          reflect =
            fun pos ->
                if (MathF.Floor(pos.Z) + MathF.Floor(pos.X)) % 2f <> 0f then
                    0.1f
                else
                    0.7f
          roughness = 150f } |}


let intersections (ray: Ray) (scene: Scene) =
    scene.things
    |> List.choose (fun thing -> intersect thing ray)
    |> List.sortBy (fun i -> i.distance)
    |> List.tryHead

let testRay (ray: Ray) (scene: Scene) =
    Option.map (fun i -> i.distance) (intersections ray scene)

let rec shade (isect: Intersection) (scene: Scene) (depth: int) =
    let getReflectionColor thing pos reflectDir scene depth =
        let surface = surface thing
        let reflectance = surface.reflect pos

        reflectance * traceRay { start = pos; direction = reflectDir } scene (depth + 1)

    let getNaturalColor thing pos normal rd scene =
        let addLight col light =
            let ldis = light.position - pos
            let livec = Vector3.Normalize(ldis)
            let neatIsect = testRay { start = pos; direction = livec } scene

            let isInShadow =
                match neatIsect with
                | Some thing -> thing <= ldis.Length()
                | None -> false

            if isInShadow then
                col
            else
                let illum = Vector3.Dot(livec, normal)

                let lcolor =
                    match illum with
                    | illum when illum > 0f -> light.color * illum
                    | _ -> black

                let specular = Vector3.Dot(livec, Vector3.Normalize(rd))

                let surface = surface thing

                let scolor =
                    match specular with
                    | specular when specular > 0f -> light.color * (specular ** surface.roughness)
                    | _ -> black

                col + (surface.diffuse pos * lcolor) + (surface.specular pos * scolor)

        List.fold addLight black scene.lights

    let d = isect.ray.direction
    let pos = isect.distance * d + isect.ray.start
    let normal = normal isect.thing pos
    let reflectDir = d - 2f * Vector3.Dot(d, normal) * normal

    let naturalColor =
        background + getNaturalColor isect.thing pos normal reflectDir scene

    let reflectedColor =
        if depth >= maxDepth then
            grey
        else
            getReflectionColor isect.thing pos reflectDir scene depth

    naturalColor + reflectedColor

and traceRay (ray: Ray) (scene: Scene) (depth: int) : Color =
    let isect = intersections ray scene

    match isect with
    | None -> background
    | Some isect -> shade isect scene depth

let render (scene: Scene) (width: int) (height: int) =
    let fw = float32 width
    let fh = float32 height

    let getPoint x y =
        let recenterX x = (x - fw / 2f) / 2f / fw

        let recenterY y = -(y - fh / 2f) / 2f / fh

        Vector3.Normalize(
            scene.camera.forward
            + (recenterX x * scene.camera.right + recenterY y * scene.camera.up)
        )

    let toDrawingColor (color: Color) =
        let legalize d = Math.Clamp(d, 0f, 1f)

        MathF.Floor(legalize color.X * 255f) |> int,
        MathF.Floor(legalize color.Y * 255f) |> int,
        MathF.Floor(legalize color.Z * 255f) |> int

    printfn $"P3\n%d{width} %d{height}\n255\n"

    for y in 0 .. height - 1 do
        let fy = float32 y

        for x in 0 .. width - 1 do
            let fx = float32 x

            let color =
                traceRay
                    { start = scene.camera.pos
                      direction = getPoint fx fy }
                    scene
                    0

            let r, g, b = toDrawingColor color

            printfn $"%d{r} %d{g} %d{b}"

[<EntryPoint>]
let main _ =
    let defaultScene =
        { things =
            [ Plane
                  { normal = Vector3(0f, 1f, 0f)
                    offset = 0f
                    surface = surfaces.checkerboard }
              Sphere
                  { center = Vector3(0f, 1f, -0.25f)
                    radius = 1f
                    surface = surfaces.shiny }
              Sphere
                  { center = Vector3(-1f, 0.5f, 1.5f)
                    radius = 0.5f
                    surface = surfaces.shiny } ]
          lights =
            [ { position = Vector3(-2f, 2.5f, 0f)
                color = Color(0.49f, 0.07f, 0.07f) }
              { position = Vector3(1.5f, 2.5f, 1.5f)
                color = Color(0.07f, 0.07f, 0.49f) }
              { position = Vector3(1.5f, 2.5f, -1.5f)
                color = Color(0.07f, 0.49f, 0.071f) }
              { position = Vector3(0f, 3.5f, 0f)
                color = Color(0.21f, 0.21f, 0.35f) } ]
          camera = mkCamera (Vector3(3f, 2f, 4f)) (Vector3(-1f, 0.5f, 0f)) }

    render defaultScene 1024 1024

    0
