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

type Sphere = { center: Vector3; radius: float32 }

type Plane = { normal: Vector3; offset: float32 }

type Intersection =
    { thing: Thing
      ray: Ray
      distance: float32 }

and Thing =
    | Sphere of Sphere * Surface
    | Plane of Plane * Surface

    member this.surface =
        match this with
        | Sphere(_, surface) -> surface
        | Plane(_, surface) -> surface

type Light = { position: Vector3; color: Color }

type Scene =
    { things: Thing list
      lights: Light list
      camera: Camera }

let intersect (thing: Thing) (ray: Ray) =
    match thing with
    | Sphere(sphere, _) ->
        let eo = sphere.center - ray.start
        let v = Vector3.Dot(eo, ray.direction)
        let mutable dist = 0f

        if v >= 0f then
            let disc = sphere.radius ** 2f - (Vector3.Dot(eo, eo) - v ** 2f)

            if disc >= 0f then
                dist <- v - sqrt disc

        if dist <> 0f then
            Some
                { thing = thing
                  ray = ray
                  distance = dist }
        else
            None
    | Plane(plane, _) ->
        let denom = Vector3.Dot(plane.normal, ray.direction)

        if denom < 0f then
            let dist = (Vector3.Dot(plane.normal, ray.start) + plane.offset) / -denom

            Some
                { thing = thing
                  ray = ray
                  distance = dist }
        else
            None

let surfaces =
    {| shiny =
        { diffuse = fun _ -> white
          specular = fun _ -> Color(0.5f)
          reflect = (fun _ -> 0.7f)
          roughness = 250f }
       checkerboard =
        { diffuse =
            fun pos ->
                if (floor pos.Z + floor pos.X) % 2f <> 0f then
                    white
                else
                    black
          specular = fun _ -> white
          reflect =
            fun pos ->
                if (floor pos.Z + floor pos.X) % 2f <> 0f then
                    0.1f
                else
                    0.7f
          roughness = 150f } |}

let intersections (ray: Ray) (scene: Scene) =
    scene.things
    |> List.choose (fun thing -> intersect thing ray)
    |> List.sortBy (fun i -> i.distance)
    |> List.tryHead

let rec shade (isect: Intersection) (scene: Scene) (depth: int) =
    let normal (thing: Thing) (pos: Vector3) =
        match thing with
        | Sphere(sphere, _) -> Vector3.Normalize(pos - sphere.center)
        | Plane(plane, _) -> plane.normal

    let testRay (ray: Ray) (scene: Scene) =
        Option.map (fun i -> i.distance) (intersections ray scene)

    let getReflectionColor (thing: Thing) (pos: Color) (reflectDir: Vector3) (scene: Scene) (depth: int) =
        let surface = thing.surface
        let reflectance = surface.reflect pos

        reflectance * traceRay { start = pos; direction = reflectDir } scene (depth + 1)

    let getNaturalColor (thing: Thing) (pos: Vector3) (normal: Vector3) (rd: Vector3) (scene: Scene) =
        let addLight (col: Vector3) (light: Light) =
            let ldis = light.position - pos
            let livec = Vector3.Normalize(ldis)
            let neatIsect = testRay { start = pos; direction = livec } scene
            let isInShadow = Option.exists (fun thing -> thing <= ldis.Length()) neatIsect

            if isInShadow then
                col
            else
                let illum = Vector3.Dot(livec, normal)
                let lcolor = if illum > 0f then light.color * illum else black
                let specular = Vector3.Dot(livec, Vector3.Normalize(rd))
                let surface = thing.surface

                let scolor =
                    if specular > 0f then
                        light.color * (specular ** surface.roughness)
                    else
                        black

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
    intersections ray scene
    |> Option.map (fun isect -> shade isect scene depth)
    |> Option.defaultValue background

let render (scene: Scene) (width: int) (height: int) =
    let fw = float32 width
    let fh = float32 height

    let getPoint (x: float32) (y: float32) =
        let recenterX x = (x - fw / 2f) / 2f / fw

        let recenterY y = -(y - fh / 2f) / 2f / fh

        Vector3.Normalize(
            scene.camera.forward
            + (recenterX x * scene.camera.right + recenterY y * scene.camera.up)
        )

    let toDrawingColor (color: Color) =
        let clamp value = Math.Clamp(value, 0f, 1f)
        let p = clamp >> (*) 255f >> floor >> int

        p color.X, p color.Y, p color.Z

    printfn $"P3\n%d{width} %d{height}\n255\n"

    Seq.allPairs { 0 .. height - 1 } { 0 .. width - 1 }
    |> Seq.map (fun (y, x) -> float32 x, float32 y)
    |> Seq.map (fun (fx, fy) -> getPoint fx fy)
    |> Seq.map (fun point ->
        traceRay
            { start = scene.camera.pos
              direction = point }
            scene
            0)
    |> Seq.map toDrawingColor
    |> Seq.iter (fun (r, g, b) -> printfn $"%d{r} %d{g} %d{b}")

[<EntryPoint>]
let main _ =
    let defaultScene =
        { things =
            [ Plane(
                  { normal = Vector3(0f, 1f, 0f)
                    offset = 0f },
                  surfaces.checkerboard
              )
              Sphere(
                  { center = Vector3(0f, 1f, -0.25f)
                    radius = 1f },
                  surfaces.shiny
              )
              Sphere(
                  { center = Vector3(-1f, 0.5f, 1.5f)
                    radius = 0.5f },
                  surfaces.shiny
              ) ]
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

    render defaultScene 256 256

    0
