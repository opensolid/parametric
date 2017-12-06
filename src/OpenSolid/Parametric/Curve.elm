module OpenSolid.Parametric.Curve exposing (..)

import OpenSolid.Arc2d as Arc2d exposing (Arc2d)
import OpenSolid.Arc3d as Arc3d exposing (Arc3d)
import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.CubicSpline2d as CubicSpline2d exposing (CubicSpline2d)
import OpenSolid.CubicSpline3d as CubicSpline3d exposing (CubicSpline3d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.LineSegment3d as LineSegment3d exposing (LineSegment3d)
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Polyline2d as Polyline2d exposing (Polyline2d)
import OpenSolid.Polyline3d as Polyline3d exposing (Polyline3d)
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d exposing (QuadraticSpline2d)
import OpenSolid.QuadraticSpline3d as QuadraticSpline3d exposing (QuadraticSpline3d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


type Curve2d
    = LineSegment2dCurve LineSegment2d
    | Arc2dCurve Arc2d
    | QuadraticSpline2dCurve QuadraticSpline2d
    | CubicSpline2dCurve CubicSpline2d
    | ProjectedCurve2d Curve3d SketchPlane3d


type Curve3d
    = LineSegment3dCurve LineSegment3d
    | Arc3dCurve Arc3d
    | QuadraticSpline3dCurve QuadraticSpline3d
    | CubicSpline3dCurve CubicSpline3d
    | PlacedCurve3d Curve2d SketchPlane3d
    | ProjectedCurve3d Curve3d Plane3d


startPoint2d : Curve2d -> Point2d
startPoint2d curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2d.startPoint lineSegment2d

        Arc2dCurve arc2d ->
            Arc2d.startPoint arc2d

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2d.startPoint quadraticSpline2d

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2d.startPoint cubicSpline2d

        ProjectedCurve2d curve3d sketchPlane ->
            startPoint3d curve3d |> Point3d.projectInto sketchPlane


endPoint2d : Curve2d -> Point2d
endPoint2d curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2d.endPoint lineSegment2d

        Arc2dCurve arc2d ->
            Arc2d.endPoint arc2d

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2d.endPoint quadraticSpline2d

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2d.endPoint cubicSpline2d

        ProjectedCurve2d curve3d sketchPlane ->
            endPoint3d curve3d |> Point3d.projectInto sketchPlane


pointOn2d : Curve2d -> Float -> Point2d
pointOn2d curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2d.interpolate lineSegment2d

        Arc2dCurve arc2d ->
            Arc2d.pointOn arc2d

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2d.pointOn quadraticSpline2d

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2d.pointOn cubicSpline2d

        ProjectedCurve2d curve3d sketchPlane ->
            pointOn3d curve3d >> Point3d.projectInto sketchPlane


evaluate2d : Curve2d -> Float -> ( Point2d, Vector2d )
evaluate2d curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            let
                derivative =
                    LineSegment2d.vector lineSegment2d
            in
            \t -> ( LineSegment2d.interpolate lineSegment2d t, derivative )

        Arc2dCurve arc2d ->
            Arc2d.evaluate arc2d

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2d.evaluate quadraticSpline2d

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2d.evaluate cubicSpline2d

        ProjectedCurve2d curve3d sketchPlane ->
            evaluate3d curve3d
                >> (\( unprojectedPoint3d, unprojectedVector3d ) ->
                        ( Point3d.projectInto sketchPlane unprojectedPoint3d
                        , Vector3d.projectInto sketchPlane unprojectedVector3d
                        )
                   )


reverse2d : Curve2d -> Curve2d
reverse2d curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2dCurve (LineSegment2d.reverse lineSegment2d)

        Arc2dCurve arc2d ->
            Arc2dCurve (Arc2d.reverse arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2dCurve (QuadraticSpline2d.reverse quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2dCurve (CubicSpline2d.reverse cubicSpline2d)

        ProjectedCurve2d curve3d sketchPlane ->
            ProjectedCurve2d (reverse3d curve3d) sketchPlane


placeIn2d : Frame2d -> Curve2d -> Curve2d
placeIn2d frame curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2dCurve (LineSegment2d.placeIn frame lineSegment2d)

        Arc2dCurve arc2d ->
            Arc2dCurve (Arc2d.placeIn frame arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2dCurve
                (QuadraticSpline2d.placeIn frame quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2dCurve (CubicSpline2d.placeIn frame cubicSpline2d)

        ProjectedCurve2d curve3d sketchPlane ->
            ProjectedCurve2d curve3d
                (Frame2d.xy
                    |> Frame2d.relativeTo frame
                    |> SketchPlane3d.on sketchPlane
                )


relativeTo2d : Frame2d -> Curve2d -> Curve2d
relativeTo2d frame curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2dCurve (LineSegment2d.relativeTo frame lineSegment2d)

        Arc2dCurve arc2d ->
            Arc2dCurve (Arc2d.relativeTo frame arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2dCurve
                (QuadraticSpline2d.relativeTo frame quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2dCurve (CubicSpline2d.relativeTo frame cubicSpline2d)

        ProjectedCurve2d curve3d sketchPlane ->
            ProjectedCurve2d curve3d (SketchPlane3d.on sketchPlane frame)


translateBy2d : Vector2d -> Curve2d -> Curve2d
translateBy2d displacement curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2dCurve
                (LineSegment2d.translateBy displacement lineSegment2d)

        Arc2dCurve arc2d ->
            Arc2dCurve
                (Arc2d.translateBy displacement arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2dCurve
                (QuadraticSpline2d.translateBy displacement quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2dCurve
                (CubicSpline2d.translateBy displacement cubicSpline2d)

        ProjectedCurve2d curve3d projectionSketchPlane ->
            let
                sketchPlaneDisplacement =
                    Vector2d.flip displacement
                        |> Vector3d.on projectionSketchPlane

                translatedSketchPlane =
                    projectionSketchPlane
                        |> SketchPlane3d.translateBy sketchPlaneDisplacement
            in
            ProjectedCurve2d curve3d translatedSketchPlane


rotateAround2d : Point2d -> Float -> Curve2d -> Curve2d
rotateAround2d point angle curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2dCurve
                (LineSegment2d.rotateAround point angle lineSegment2d)

        Arc2dCurve arc2d ->
            Arc2dCurve
                (Arc2d.rotateAround point angle arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2dCurve
                (QuadraticSpline2d.rotateAround point angle quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2dCurve
                (CubicSpline2d.rotateAround point angle cubicSpline2d)

        ProjectedCurve2d curve3d projectionSketchPlane ->
            let
                rotationAxis =
                    Axis3d.with
                        { originPoint =
                            Point3d.on projectionSketchPlane point
                        , direction =
                            SketchPlane3d.normalDirection projectionSketchPlane
                        }

                rotatedSketchPlane =
                    projectionSketchPlane
                        |> SketchPlane3d.rotateAround rotationAxis -angle
            in
            ProjectedCurve2d curve3d rotatedSketchPlane


mirrorAcross2d : Axis2d -> Curve2d -> Curve2d
mirrorAcross2d axis curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2dCurve
                (LineSegment2d.mirrorAcross axis lineSegment2d)

        Arc2dCurve arc2d ->
            Arc2dCurve
                (Arc2d.mirrorAcross axis arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2dCurve
                (QuadraticSpline2d.mirrorAcross axis quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2dCurve
                (CubicSpline2d.mirrorAcross axis cubicSpline2d)

        ProjectedCurve2d curve3d projectionSketchPlane ->
            let
                mirroredSketchPlane =
                    Frame2d.xy
                        |> Frame2d.mirrorAcross axis
                        |> SketchPlane3d.on projectionSketchPlane
            in
            ProjectedCurve2d curve3d mirroredSketchPlane


maxSecondDerivativeMagnitude2d : Curve2d -> Float
maxSecondDerivativeMagnitude2d curve2d =
    case curve2d of
        LineSegment2dCurve _ ->
            0

        Arc2dCurve arc2d ->
            let
                sweptAngle =
                    Arc2d.sweptAngle arc2d
            in
            Arc2d.radius arc2d * sweptAngle * sweptAngle

        QuadraticSpline2dCurve quadraticSpline2d ->
            let
                ( p1, p2, p3 ) =
                    QuadraticSpline2d.controlPoints quadraticSpline2d

                v1 =
                    Vector2d.from p1 p2

                v2 =
                    Vector2d.from p2 p3
            in
            2 * Vector2d.length (Vector2d.difference v2 v1)

        CubicSpline2dCurve cubicSpline2d ->
            let
                ( p1, p2, p3, p4 ) =
                    CubicSpline2d.controlPoints cubicSpline2d

                u1 =
                    Vector2d.from p1 p2

                u2 =
                    Vector2d.from p2 p3

                u3 =
                    Vector2d.from p3 p4

                v1 =
                    Vector2d.difference u2 u1

                v2 =
                    Vector2d.difference u3 u2
            in
            6 * max (Vector2d.length v1) (Vector2d.length v2)

        ProjectedCurve2d curve3d _ ->
            maxSecondDerivativeMagnitude3d curve3d


numSegments : Float -> Float -> Int
numSegments tolerance maxSecondDerivativeMagnitude =
    if tolerance > 0 then
        max 1 (ceiling (sqrt (maxSecondDerivativeMagnitude / (8 * tolerance))))
    else
        1


numSegments2d : Float -> Curve2d -> Int
numSegments2d tolerance curve2d =
    numSegments tolerance (maxSecondDerivativeMagnitude2d curve2d)


evenlySpacedParameterValues : Int -> List Float
evenlySpacedParameterValues numSegments =
    List.range 0 numSegments
        |> List.map (\index -> toFloat index / toFloat numSegments)


toPolyline2d : Float -> Curve2d -> Polyline2d
toPolyline2d tolerance curve2d =
    let
        numSegments =
            numSegments2d tolerance curve2d

        parameterValues =
            evenlySpacedParameterValues numSegments

        points =
            parameterValues |> List.map (pointOn2d curve2d)
    in
    Polyline2d.fromVertices points


samples2d : Float -> Curve2d -> List ( Point2d, Vector2d )
samples2d tolerance curve2d =
    let
        numSegments =
            numSegments2d tolerance curve2d

        parameterValues =
            evenlySpacedParameterValues numSegments
    in
    parameterValues |> List.map (evaluate2d curve2d)


on3d : SketchPlane3d -> Curve2d -> Curve3d
on3d sketchPlane curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment3dCurve
                (LineSegment3d.on sketchPlane lineSegment2d)

        Arc2dCurve arc2d ->
            Arc3dCurve
                (Arc3d.on sketchPlane arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline3dCurve
                (QuadraticSpline3d.on sketchPlane quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline3dCurve
                (CubicSpline3d.on sketchPlane cubicSpline2d)

        ProjectedCurve2d curve3d projectionSketchPlane ->
            if sketchPlane == projectionSketchPlane then
                curve3d
            else
                PlacedCurve3d curve2d sketchPlane


startPoint3d : Curve3d -> Point3d
startPoint3d curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            LineSegment3d.startPoint lineSegment3d

        Arc3dCurve arc3d ->
            Arc3d.startPoint arc3d

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3d.startPoint quadraticSpline3d

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3d.startPoint cubicSpline3d

        PlacedCurve3d curve2d sketchPlane ->
            startPoint2d curve2d |> Point3d.on sketchPlane

        ProjectedCurve3d unprojectedCurve3d plane ->
            startPoint3d unprojectedCurve3d |> Point3d.projectOnto plane


endPoint3d : Curve3d -> Point3d
endPoint3d curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            LineSegment3d.endPoint lineSegment3d

        Arc3dCurve arc3d ->
            Arc3d.endPoint arc3d

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3d.endPoint quadraticSpline3d

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3d.endPoint cubicSpline3d

        PlacedCurve3d curve2d sketchPlane ->
            endPoint2d curve2d |> Point3d.on sketchPlane

        ProjectedCurve3d unprojectedCurve3d plane ->
            endPoint3d unprojectedCurve3d |> Point3d.projectOnto plane


pointOn3d : Curve3d -> Float -> Point3d
pointOn3d curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            LineSegment3d.interpolate lineSegment3d

        Arc3dCurve arc3d ->
            Arc3d.pointOn arc3d

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3d.pointOn quadraticSpline3d

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3d.pointOn cubicSpline3d

        PlacedCurve3d curve2d sketchPlane ->
            pointOn2d curve2d >> Point3d.on sketchPlane

        ProjectedCurve3d unprojectedCurve3d plane ->
            pointOn3d unprojectedCurve3d >> Point3d.projectOnto plane


evaluate3d : Curve3d -> Float -> ( Point3d, Vector3d )
evaluate3d curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            let
                derivative =
                    LineSegment3d.vector lineSegment3d
            in
            \t -> ( LineSegment3d.interpolate lineSegment3d t, derivative )

        Arc3dCurve arc3d ->
            Arc3d.evaluate arc3d

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3d.evaluate quadraticSpline3d

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3d.evaluate cubicSpline3d

        PlacedCurve3d curve2d sketchPlane ->
            evaluate2d curve2d
                >> (\( point2d, vector2d ) ->
                        ( Point3d.on sketchPlane point2d
                        , Vector3d.on sketchPlane vector2d
                        )
                   )

        ProjectedCurve3d unprojectedCurve3d plane ->
            evaluate3d unprojectedCurve3d
                >> (\( unprojectedPoint3d, unprojectedVector3d ) ->
                        ( Point3d.projectOnto plane unprojectedPoint3d
                        , Vector3d.projectOnto plane unprojectedVector3d
                        )
                   )


reverse3d : Curve3d -> Curve3d
reverse3d curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            LineSegment3dCurve (LineSegment3d.reverse lineSegment3d)

        Arc3dCurve arc3d ->
            Arc3dCurve (Arc3d.reverse arc3d)

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3dCurve (QuadraticSpline3d.reverse quadraticSpline3d)

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3dCurve (CubicSpline3d.reverse cubicSpline3d)

        PlacedCurve3d curve2d sketchPlane ->
            PlacedCurve3d (reverse2d curve2d) sketchPlane

        ProjectedCurve3d unprojectedCurve3d plane ->
            ProjectedCurve3d (reverse3d unprojectedCurve3d) plane


projectOnto : Plane3d -> Curve3d -> Curve3d
projectOnto plane curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            LineSegment3dCurve
                (LineSegment3d.projectOnto plane lineSegment3d)

        Arc3dCurve _ ->
            ProjectedCurve3d curve3d plane

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3dCurve
                (QuadraticSpline3d.projectOnto plane quadraticSpline3d)

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3dCurve
                (CubicSpline3d.projectOnto plane cubicSpline3d)

        PlacedCurve3d _ _ ->
            ProjectedCurve3d curve3d plane

        ProjectedCurve3d unprojectedCurve3d originalProjectionPlane ->
            if plane == originalProjectionPlane then
                curve3d
            else
                ProjectedCurve3d curve3d plane


placeIn3d : Frame3d -> Curve3d -> Curve3d
placeIn3d frame curve =
    case curve of
        LineSegment3dCurve lineSegment ->
            LineSegment3dCurve (LineSegment3d.placeIn frame lineSegment)

        Arc3dCurve arc ->
            Arc3dCurve (Arc3d.placeIn frame arc)

        QuadraticSpline3dCurve quadraticSpline ->
            QuadraticSpline3dCurve
                (QuadraticSpline3d.placeIn frame quadraticSpline)

        CubicSpline3dCurve cubicSpline ->
            CubicSpline3dCurve
                (CubicSpline3d.placeIn frame cubicSpline)

        PlacedCurve3d curve2d sketchPlane ->
            PlacedCurve3d curve2d (SketchPlane3d.placeIn frame sketchPlane)

        ProjectedCurve3d unprojectedCurve projectionPlane ->
            ProjectedCurve3d
                (placeIn3d frame unprojectedCurve)
                (Plane3d.placeIn frame projectionPlane)


relativeTo3d : Frame3d -> Curve3d -> Curve3d
relativeTo3d frame curve =
    case curve of
        LineSegment3dCurve lineSegment ->
            LineSegment3dCurve (LineSegment3d.relativeTo frame lineSegment)

        Arc3dCurve arc ->
            Arc3dCurve (Arc3d.relativeTo frame arc)

        QuadraticSpline3dCurve quadraticSpline ->
            QuadraticSpline3dCurve
                (QuadraticSpline3d.relativeTo frame quadraticSpline)

        CubicSpline3dCurve cubicSpline ->
            CubicSpline3dCurve
                (CubicSpline3d.relativeTo frame cubicSpline)

        PlacedCurve3d curve2d sketchPlane ->
            PlacedCurve3d curve2d (SketchPlane3d.relativeTo frame sketchPlane)

        ProjectedCurve3d unprojectedCurve projectionPlane ->
            ProjectedCurve3d
                (relativeTo3d frame unprojectedCurve)
                (Plane3d.relativeTo frame projectionPlane)


rotateAround3d : Axis3d -> Float -> Curve3d -> Curve3d
rotateAround3d axis angle curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment ->
            LineSegment3dCurve
                (LineSegment3d.rotateAround axis angle lineSegment)

        Arc3dCurve arc ->
            Arc3dCurve (Arc3d.rotateAround axis angle arc)

        QuadraticSpline3dCurve quadraticSpline ->
            QuadraticSpline3dCurve
                (QuadraticSpline3d.rotateAround axis angle quadraticSpline)

        CubicSpline3dCurve cubicSpline ->
            CubicSpline3dCurve
                (CubicSpline3d.rotateAround axis angle cubicSpline)

        PlacedCurve3d curve2d sketchPlane ->
            PlacedCurve3d curve2d
                (SketchPlane3d.rotateAround axis angle sketchPlane)

        ProjectedCurve3d unprojectedCurve projectionPlane ->
            ProjectedCurve3d
                (rotateAround3d axis angle unprojectedCurve)
                (Plane3d.rotateAround axis angle projectionPlane)


translateBy3d : Vector3d -> Curve3d -> Curve3d
translateBy3d displacement curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment ->
            LineSegment3dCurve
                (LineSegment3d.translateBy displacement lineSegment)

        Arc3dCurve arc ->
            Arc3dCurve (Arc3d.translateBy displacement arc)

        QuadraticSpline3dCurve quadraticSpline ->
            QuadraticSpline3dCurve
                (QuadraticSpline3d.translateBy displacement quadraticSpline)

        CubicSpline3dCurve cubicSpline ->
            CubicSpline3dCurve
                (CubicSpline3d.translateBy displacement cubicSpline)

        PlacedCurve3d curve2d sketchPlane ->
            PlacedCurve3d curve2d
                (SketchPlane3d.translateBy displacement sketchPlane)

        ProjectedCurve3d unprojectedCurve projectionPlane ->
            ProjectedCurve3d
                (translateBy3d displacement unprojectedCurve)
                (Plane3d.translateBy displacement projectionPlane)


maxSecondDerivativeMagnitude3d : Curve3d -> Float
maxSecondDerivativeMagnitude3d curve3d =
    case curve3d of
        LineSegment3dCurve _ ->
            0

        Arc3dCurve arc3d ->
            let
                sweptAngle =
                    Arc3d.sweptAngle arc3d
            in
            Arc3d.radius arc3d * sweptAngle * sweptAngle

        QuadraticSpline3dCurve quadraticSpline3d ->
            let
                ( p1, p2, p3 ) =
                    QuadraticSpline3d.controlPoints quadraticSpline3d

                v1 =
                    Vector3d.from p1 p2

                v2 =
                    Vector3d.from p2 p3
            in
            2 * Vector3d.length (Vector3d.difference v2 v1)

        CubicSpline3dCurve cubicSpline3d ->
            let
                ( p1, p2, p3, p4 ) =
                    CubicSpline3d.controlPoints cubicSpline3d

                u1 =
                    Vector3d.from p1 p2

                u2 =
                    Vector3d.from p2 p3

                u3 =
                    Vector3d.from p3 p4

                v1 =
                    Vector3d.difference u2 u1

                v2 =
                    Vector3d.difference u3 u2
            in
            6 * max (Vector3d.length v1) (Vector3d.length v2)

        ProjectedCurve3d unprojectedCurve3d _ ->
            maxSecondDerivativeMagnitude3d unprojectedCurve3d

        PlacedCurve3d curve2d _ ->
            maxSecondDerivativeMagnitude2d curve2d


numSegments3d : Float -> Curve3d -> Int
numSegments3d tolerance curve3d =
    numSegments tolerance (maxSecondDerivativeMagnitude3d curve3d)


toPolyline3d : Float -> Curve3d -> Polyline3d
toPolyline3d tolerance curve3d =
    let
        numSegments =
            numSegments3d tolerance curve3d

        parameterValues =
            evenlySpacedParameterValues numSegments

        points =
            parameterValues |> List.map (pointOn3d curve3d)
    in
    Polyline3d.fromVertices points


samples3d : Float -> Curve3d -> List ( Point3d, Vector3d )
samples3d tolerance curve3d =
    let
        numSegments =
            numSegments3d tolerance curve3d

        parameterValues =
            evenlySpacedParameterValues numSegments
    in
    parameterValues |> List.map (evaluate3d curve3d)
