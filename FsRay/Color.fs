namespace FsRay

open System.Numerics

type Color = Vector3

module Color =
    let white: Color = Color.One

    let grey = Color(0.5f)

    let black = Color.Zero

    let background = black

    let defaultColor = black
