namespace FsRay

open System.Numerics

type Surface =
    { diffuse: Color -> Color
      specular: Color
      reflect: Color -> float32
      roughness: float32 }

module Surface =
    let shiny =
        let always x _ = x

        { diffuse = always Color.white
          specular = Color.grey
          reflect = always 0.7f
          roughness = 250f }

    let checkerboard =
        let ifWhite (pos: Vector3) (a: 'a) (b: 'a) =
            if (floor pos.Z + floor pos.X) % 2f <> 0f then a else b

        { diffuse = fun pos -> ifWhite pos Color.white Color.black
          specular = Color.white
          reflect = fun pos -> ifWhite pos 0.1f 0.7f
          roughness = 150f }
