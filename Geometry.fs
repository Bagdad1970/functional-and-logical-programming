namespace Geometry
open System

module GeometryShapes =

    type IPrint = interface
        abstract member Print : unit -> unit
        end


    [<AbstractClass>]
    type GeometricShape() =
        abstract member Area : float
        override this.ToString() = "Geometric Shape"


    type Rectangle(width: float, height: float) =
        inherit GeometricShape()

        let mutable _width = width
        let mutable _height = height
            
        interface IPrint with
            member this.Print() = printfn "%s" (this.ToString())

        
        member this.Width 
            with get() = _width
            and set(value) = _width <- value
            
        member this.Height 
            with get() = _height
            and set(value) = _height <- value
        
        override this.Area = this.Width * this.Height
        
        override this.ToString() = 
            sprintf "Rectangle: Width = %.2f, Height = %.2f, Area = %.2f" 
                this.Width this.Height this.Area
        

    type Square(side: float) =
        inherit Rectangle(side, side)
        
        override this.ToString() = 
            sprintf "Square: Side = %.2f, Area = %.2f" 
                this.Width this.Area
        
        interface IPrint with
            member this.Print() = printfn "%s" (this.ToString())

    type Circle(radius: float) =
        inherit GeometricShape()

        let mutable _radius = radius

        
        member this.Radius 
            with get() = _radius
            and set(value) = _radius <- value
        
        override this.Area = System.Math.PI * this.Radius * this.Radius
        
        override this.ToString() = 
            sprintf "Circle: Radius = %.2f, Area = %.2f" 
                this.Radius this.Area
        
        interface IPrint with
            member this.Print() = printfn "%s" (this.ToString())