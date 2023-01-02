namespace FsRay

type Surface =
    { diffuse: Color -> Color
      specular: Color -> Color
      reflect: Color -> float32
      roughness: float32 }

module Surface =
    let shiny =
        { diffuse = fun _ -> Color.white
          specular = fun _ -> Color(0.5f)
          reflect = (fun _ -> 0.7f)
          roughness = 250f }

    let checkerboard =
        { diffuse =
            fun pos ->
                if (floor pos.Z + floor pos.X) % 2f <> 0f then
                    Color.white
                else
                    Color.black
          specular = fun _ -> Color.white
          reflect =
            fun pos ->
                if (floor pos.Z + floor pos.X) % 2f <> 0f then
                    0.1f
                else
                    0.7f
          roughness = 150f }
