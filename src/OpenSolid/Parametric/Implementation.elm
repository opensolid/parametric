module OpenSolid.Parametric.Implementation exposing (..)

import OpenSolid.Arc2d as Arc2d
import OpenSolid.Arc3d as Arc3d
import OpenSolid.CubicSpline2d as CubicSpline2d
import OpenSolid.CubicSpline3d as CubicSpline3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Point2d as Point2d
import OpenSolid.Point3d as Point3d
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d
import OpenSolid.QuadraticSpline3d as QuadraticSpline3d


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


curve2dPointOn : Curve2d -> Float -> Point2d
curve2dPointOn curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2d.interpolate lineSegment2d

        Arc2dCurve arc2d ->
            Arc2d.point arc2d

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2d.point quadraticSpline2d

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2d.point cubicSpline2d

        ProjectedCurve2d curve3d sketchPlane ->
            curve3dPointOn curve3d >> Point3d.projectInto sketchPlane


curve2dPlaceOnto : SketchPlane3d -> Curve2d -> Curve3d
curve2dPlaceOnto sketchPlane curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment3dCurve
                (LineSegment2d.placeOnto sketchPlane lineSegment2d)

        Arc2dCurve arc2d ->
            Arc3dCurve
                (Arc2d.placeOnto sketchPlane arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline3dCurve
                (QuadraticSpline2d.placeOnto sketchPlane quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline3dCurve
                (CubicSpline2d.placeOnto sketchPlane cubicSpline2d)

        ProjectedCurve2d curve3d projectionSketchPlane ->
            if sketchPlane == projectionSketchPlane then
                curve3d
            else
                PlacedCurve3d curve2d sketchPlane


curve3dPointOn : Curve3d -> Float -> Point3d
curve3dPointOn curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            LineSegment3d.interpolate lineSegment3d

        Arc3dCurve arc3d ->
            Arc3d.point arc3d

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3d.point quadraticSpline3d

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3d.point cubicSpline3d

        PlacedCurve3d curve2d sketchPlane ->
            curve2dPointOn curve2d >> Point2d.placeOnto sketchPlane

        ProjectedCurve3d unprojectedCurve3d plane ->
            curve3dPointOn unprojectedCurve3d >> Point3d.projectOnto plane


curve3dProjectOnto : Plane3d -> Curve3d -> Curve3d
curve3dProjectOnto plane curve3d =
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
