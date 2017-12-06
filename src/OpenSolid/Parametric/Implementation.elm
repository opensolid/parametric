module OpenSolid.Parametric.Implementation exposing (..)

import Array.Hamt as Array
import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Mesh as Mesh exposing (Mesh)
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Rectangle2d as Rectangle2d exposing (Rectangle2d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


type Surface3d
    = Surface3d Bool ParametricSurface3d


type ParametricSurface3d
    = ExtrusionSurface Curve3d Vector3d
    | RevolutionSurface Curve3d Frame3d Float
    | ParallelogramSurface Point3d Vector3d Vector3d
    | PlanarSurface Region2d SketchPlane3d


type BoundaryType
    = Interior
    | Exterior


type Region2d
    = RectangleRegion Rectangle2d { left : BoundaryType, right : BoundaryType, top : BoundaryType, bottom : BoundaryType }
    | ExtrusionRegion Curve2d Vector2d { start : BoundaryType, end : BoundaryType, left : BoundaryType, right : BoundaryType }
    | RevolutionRegion Curve2d Point2d Float { start : BoundaryType, end : BoundaryType, inside : BoundaryType, outside : BoundaryType }
    | FanRegion Point2d Curve2d { start : BoundaryType, end : BoundaryType, curve : BoundaryType }
    | Fused (List Region2d)


type Body3d
    = Body3d (List Surface3d)


surface3dExtrusion : Curve3d -> Vector3d -> Surface3d
surface3dExtrusion curve vector =
    case curve of
        LineSegment3dCurve lineSegment3d ->
            let
                ( p0, p1 ) =
                    LineSegment3d.endpoints lineSegment3d
            in
            Surface3d True
                (ParallelogramSurface p0 (Vector3d.from p0 p1) vector)

        _ ->
            Surface3d True (ExtrusionSurface curve vector)


surface3dRevolution : Curve3d -> Axis3d -> Float -> Surface3d
surface3dRevolution curve axis angle =
    let
        frame =
            Frame3d.with
                { originPoint = Axis3d.originPoint axis
                , zDirection = Axis3d.direction axis
                }
    in
    Surface3d True
        (RevolutionSurface (curve3dRelativeTo frame curve) frame angle)


surface3dPlanar : Region2d -> SketchPlane3d -> Surface3d
surface3dPlanar region sketchPlane =
    Surface3d True (PlanarSurface region sketchPlane)


surface3dFlip : Surface3d -> Surface3d
surface3dFlip (Surface3d isRightHanded surface) =
    Surface3d (not isRightHanded) surface


surface3dPointOn : Surface3d -> Point2d -> Point3d
surface3dPointOn (Surface3d _ surface) =
    case surface of
        ParallelogramSurface point uVector vVector ->
            let
                ( x0, y0, z0 ) =
                    Point3d.coordinates point

                ( xu, yu, zu ) =
                    Vector3d.components uVector

                ( xv, yv, zv ) =
                    Vector3d.components vVector
            in
            \point ->
                let
                    ( u, v ) =
                        Point2d.coordinates point
                in
                Point3d.fromCoordinates
                    ( x0 + u * xu + v * xv
                    , y0 + u * yu + v * yv
                    , z0 + u * zu + v * zv
                    )

        ExtrusionSurface curve vector ->
            let
                pointOnCurve =
                    curve3dPointOn curve
            in
            \point ->
                let
                    ( u, v ) =
                        Point2d.coordinates point
                in
                pointOnCurve u
                    |> Point3d.translateBy (Vector3d.scaleBy v vector)

        RevolutionSurface localCurve frame angle ->
            let
                pointOnCurve =
                    curve3dPointOn localCurve
            in
            \point ->
                let
                    ( u, v ) =
                        Point2d.coordinates point

                    ( x0, y0, z ) =
                        Point3d.coordinates (pointOnCurve v)

                    theta =
                        u * angle

                    cosTheta =
                        cos theta

                    sinTheta =
                        sin theta

                    x =
                        x0 * cosTheta - y0 * sinTheta

                    y =
                        y0 * cosTheta + x0 * sinTheta
                in
                Point3d.in_ frame ( x, y, z )

        PlanarSurface _ sketchPlane ->
            -- TODO: interpolate within region bounding box?
            Point3d.on sketchPlane


surface3dToMesh : Float -> Surface3d -> Mesh ( Point3d, Vector3d )
surface3dToMesh tolerance (Surface3d isRightHanded surface3d) =
    case surface3d of
        ParallelogramSurface point uVector vVector ->
            let
                n =
                    Vector3d.normalize <|
                        if isRightHanded then
                            Vector3d.crossProduct uVector vVector
                        else
                            Vector3d.crossProduct vVector uVector

                p0 =
                    point

                p1 =
                    p0 |> Point3d.translateBy uVector

                p2 =
                    p1 |> Point3d.translateBy vVector

                p3 =
                    p0 |> Point3d.translateBy vVector
            in
            Mesh.with
                { vertices =
                    Array.fromList
                        [ ( p0, n ), ( p1, n ), ( p2, n ), ( p3, n ) ]
                , faceIndices =
                    if isRightHanded then
                        [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
                    else
                        [ ( 0, 2, 1 ), ( 0, 3, 2 ) ]
                }

        ExtrusionSurface curve3d extrusionVector ->
            let
                curveSamples =
                    curve3dSamples tolerance curve3d

                toVertex ( curvePoint, curveDerivative ) =
                    let
                        normalVector =
                            Vector3d.normalize <|
                                if isRightHanded then
                                    Vector3d.crossProduct
                                        curveDerivative
                                        extrusionVector
                                else
                                    Vector3d.crossProduct
                                        extrusionVector
                                        curveDerivative
                    in
                    ( curvePoint, normalVector )

                startVertices =
                    List.map toVertex curveSamples

                toEndVertex ( startPoint, startNormal ) =
                    ( Point3d.translateBy extrusionVector startPoint
                    , startNormal
                    )

                endVertices =
                    List.map toEndVertex startVertices

                numColumns =
                    List.length startVertices - 1

                accumulate startVertices endVertices result =
                    case ( startVertices, endVertices ) of
                        ( startFirst :: startRest, endFirst :: endRest ) ->
                            accumulate
                                startRest
                                endRest
                                (startFirst :: endFirst :: result)

                        _ ->
                            result

                prependFaces columnIndex faces =
                    if columnIndex < numColumns then
                        let
                            offset =
                                2 * columnIndex

                            faceIndices1 =
                                if isRightHanded then
                                    ( offset + 2, offset, offset + 1 )
                                else
                                    ( offset + 2, offset + 1, offset )

                            faceIndices2 =
                                if isRightHanded then
                                    ( offset + 2, offset + 1, offset + 3 )
                                else
                                    ( offset + 2, offset + 3, offset + 1 )
                        in
                        prependFaces (columnIndex + 1)
                            (faceIndices1 :: faceIndices2 :: faces)
                    else
                        faces
            in
            Mesh.with
                { vertices =
                    Array.fromList (accumulate startVertices endVertices [])
                , faceIndices = prependFaces 0 []
                }

        RevolutionSurface localCurve3d frame sweptAngle ->
            let
                curveSamples =
                    curve3dSamples tolerance localCurve3d

                squaredRadius point =
                    let
                        ( x, y, _ ) =
                            Point3d.coordinates point
                    in
                    x * x + y * y

                maxRadius =
                    sqrt <|
                        List.foldl
                            (\( point, derivative ) currentMax ->
                                max currentMax (squaredRadius point)
                            )
                            0
                            curveSamples

                maxSecondDerivativeMagnitude =
                    maxRadius * sweptAngle * sweptAngle

                numRotationSteps =
                    curveNumSegments tolerance maxSecondDerivativeMagnitude

                toStartVertex ( point, vDerivative ) =
                    let
                        ( x, y, _ ) =
                            Point3d.coordinates point

                        uDerivative =
                            Vector3d.fromComponents
                                ( -sweptAngle * y
                                , sweptAngle * x
                                , 0
                                )

                        normalVector =
                            Vector3d.normalize <|
                                if isRightHanded then
                                    Vector3d.crossProduct
                                        vDerivative
                                        uDerivative
                                else
                                    Vector3d.crossProduct
                                        uDerivative
                                        vDerivative
                    in
                    ( point, normalVector )

                startVertices =
                    List.map toStartVertex curveSamples

                rotateVertexBy angle =
                    let
                        cosAngle =
                            cos angle

                        sinAngle =
                            sin angle
                    in
                    \( point, normalVector ) ->
                        let
                            ( x, y, z ) =
                                Point3d.coordinates point

                            ( nx, ny, nz ) =
                                Vector3d.components normalVector
                        in
                        ( Point3d.fromCoordinates
                            ( x * cosAngle - y * sinAngle
                            , y * cosAngle + x * sinAngle
                            , z
                            )
                        , Vector3d.fromComponents
                            ( nx * cosAngle - ny * sinAngle
                            , ny * cosAngle + nx * sinAngle
                            , nz
                            )
                        )

                rotationAngles =
                    List.range 1 numRotationSteps
                        |> List.map
                            (\index ->
                                sweptAngle
                                    * (toFloat index / toFloat numRotationSteps)
                            )

                rotatedVertexLists =
                    rotationAngles
                        |> List.map
                            (\angle ->
                                startVertices |> List.map (rotateVertexBy angle)
                            )

                vertices =
                    List.concat (startVertices :: rotatedVertexLists)
                        |> List.map
                            (\( position, normal ) ->
                                ( Point3d.placeIn frame position
                                , Vector3d.placeIn frame normal
                                )
                            )

                numColumns =
                    List.length curveSamples - 1

                numRows =
                    numRotationSteps

                prependFaces rowIndex columnIndex faces =
                    if columnIndex < numColumns then
                        let
                            i1 =
                                rowIndex * (numColumns + 1) + columnIndex

                            i2 =
                                i1 + 1

                            i3 =
                                i2 + numColumns + 1

                            i4 =
                                i3 - 1

                            faceIndices1 =
                                if isRightHanded then
                                    ( i1, i2, i3 )
                                else
                                    ( i1, i3, i2 )

                            faceIndices2 =
                                if isRightHanded then
                                    ( i1, i3, i4 )
                                else
                                    ( i1, i4, i3 )
                        in
                        prependFaces
                            rowIndex
                            (columnIndex + 1)
                            (faceIndices1 :: faceIndices2 :: faces)
                    else if rowIndex < (numRows - 1) then
                        prependFaces (rowIndex + 1) 0 faces
                    else
                        faces
            in
            Mesh.with
                { vertices =
                    List.concat (startVertices :: rotatedVertexLists)
                        |> List.map
                            (\( position, normal ) ->
                                ( Point3d.placeIn frame position
                                , Vector3d.placeIn frame normal
                                )
                            )
                        |> Array.fromList
                , faceIndices = prependFaces 0 0 []
                }

        PlanarSurface region sketchPlane ->
            let
                planeNormalVector =
                    SketchPlane3d.normalDirection sketchPlane
                        |> Direction3d.toVector

                normalVector =
                    if isRightHanded then
                        planeNormalVector
                    else
                        Vector3d.flip planeNormalVector

                toVertex3d point =
                    ( Point3d.on sketchPlane point
                    , normalVector
                    )

                rightHandedMesh =
                    regionToMesh tolerance region |> Mesh.mapVertices toVertex3d
            in
            if isRightHanded then
                rightHandedMesh
            else
                let
                    vertices =
                        Mesh.vertices rightHandedMesh

                    rightHandedFaceIndices =
                        Mesh.faceIndices rightHandedMesh

                    flipFaceOrientation ( i, j, k ) =
                        ( i, k, j )

                    leftHandedFaceIndices =
                        List.map flipFaceOrientation rightHandedFaceIndices
                in
                Mesh.with
                    { vertices = vertices
                    , faceIndices = leftHandedFaceIndices
                    }


surface3dTranslateBy : Vector3d -> Surface3d -> Surface3d
surface3dTranslateBy displacement (Surface3d isRightHanded surface) =
    case surface of
        ExtrusionSurface curve3d extrusionVector ->
            Surface3d isRightHanded <|
                ExtrusionSurface
                    (curve3dTranslateBy displacement curve3d)
                    extrusionVector

        RevolutionSurface localCurve3d frame sweptAngle ->
            Surface3d isRightHanded <|
                RevolutionSurface
                    localCurve3d
                    (Frame3d.translateBy displacement frame)
                    sweptAngle

        ParallelogramSurface point uVector vVector ->
            Surface3d isRightHanded <|
                ParallelogramSurface
                    (Point3d.translateBy displacement point)
                    uVector
                    vVector

        PlanarSurface region sketchPlane ->
            Surface3d isRightHanded <|
                PlanarSurface
                    region
                    (SketchPlane3d.translateBy displacement sketchPlane)


surface3dRotateAround : Axis3d -> Float -> Surface3d -> Surface3d
surface3dRotateAround axis angle (Surface3d isRightHanded surface) =
    case surface of
        ExtrusionSurface curve3d extrusionVector ->
            Surface3d isRightHanded <|
                ExtrusionSurface
                    (curve3dRotateAround axis angle curve3d)
                    (Vector3d.rotateAround axis angle extrusionVector)

        RevolutionSurface localCurve3d frame sweptAngle ->
            Surface3d isRightHanded <|
                RevolutionSurface
                    localCurve3d
                    (Frame3d.rotateAround axis angle frame)
                    sweptAngle

        ParallelogramSurface point uVector vVector ->
            let
                rotateVector =
                    Vector3d.rotateAround axis angle
            in
            Surface3d isRightHanded <|
                ParallelogramSurface
                    (Point3d.rotateAround axis angle point)
                    (rotateVector uVector)
                    (rotateVector vVector)

        PlanarSurface region sketchPlane ->
            Surface3d isRightHanded <|
                PlanarSurface
                    region
                    (SketchPlane3d.rotateAround axis angle sketchPlane)


regionExtrusionWith : { start : BoundaryType, end : BoundaryType, left : BoundaryType, right : BoundaryType } -> Curve2d -> Vector2d -> Region2d
regionExtrusionWith edgeTypes curve2d extrusionVector =
    let
        startPoint =
            curve2dStartPoint curve2d

        endPoint =
            curve2dEndPoint curve2d

        crossProduct =
            Vector2d.crossProduct
                (Vector2d.from startPoint endPoint)
                extrusionVector

        leftToRight =
            crossProduct >= 0

        startCurve =
            if leftToRight then
                curve2d
            else
                curve2dReverse curve2d
    in
    ExtrusionRegion startCurve extrusionVector edgeTypes


regionRevolutionWith : { start : BoundaryType, end : BoundaryType, inside : BoundaryType, outside : BoundaryType } -> Curve2d -> Point2d -> Float -> Region2d
regionRevolutionWith edgeTypes curve2d centerPoint sweptAngle =
    let
        startPoint =
            curve2dStartPoint curve2d

        endPoint =
            curve2dEndPoint curve2d

        startSquaredRadius =
            Point2d.squaredDistanceFrom centerPoint startPoint

        endSquaredRadius =
            Point2d.squaredDistanceFrom centerPoint endPoint

        insideToOutside =
            startSquaredRadius <= endSquaredRadius

        startCurve =
            if insideToOutside then
                curve2d
            else
                curve2dReverse curve2d
    in
    RevolutionRegion startCurve centerPoint sweptAngle edgeTypes


regionBoundaries : Region2d -> List Curve2d
regionBoundaries region =
    case region of
        RectangleRegion rectangle edgeTypes ->
            let
                ( p1, p2, p3, p4 ) =
                    Rectangle2d.vertices rectangle

                boundaryCurve edgeType startPoint endPoint =
                    case edgeType of
                        Exterior ->
                            Just <|
                                LineSegment2dCurve <|
                                    LineSegment2d.from startPoint endPoint

                        Interior ->
                            Nothing
            in
            if Frame2d.isRightHanded (Rectangle2d.axes rectangle) then
                List.filterMap identity
                    [ boundaryCurve edgeTypes.bottom p1 p2
                    , boundaryCurve edgeTypes.right p2 p3
                    , boundaryCurve edgeTypes.top p3 p4
                    , boundaryCurve edgeTypes.left p4 p1
                    ]
            else
                List.filterMap identity
                    [ boundaryCurve edgeTypes.bottom p2 p1
                    , boundaryCurve edgeTypes.right p3 p2
                    , boundaryCurve edgeTypes.top p4 p3
                    , boundaryCurve edgeTypes.left p1 p4
                    ]

        ExtrusionRegion curve2d extrusionVector edgeTypes ->
            let
                startCurve =
                    case edgeTypes.start of
                        Exterior ->
                            Just curve2d

                        Interior ->
                            Nothing

                endCurve =
                    case edgeTypes.end of
                        Exterior ->
                            Just
                                (curve2d
                                    |> curve2dTranslateBy extrusionVector
                                    |> curve2dReverse
                                )

                        Interior ->
                            Nothing

                startPoint =
                    curve2dStartPoint curve2d

                endPoint =
                    curve2dEndPoint curve2d

                leftCurve =
                    case edgeTypes.left of
                        Exterior ->
                            let
                                lineSegment =
                                    LineSegment2d.fromEndpoints
                                        ( startPoint
                                        , Point2d.translateBy
                                            extrusionVector
                                            startPoint
                                        )
                            in
                            Just (LineSegment2dCurve lineSegment)

                        Interior ->
                            Nothing

                rightCurve =
                    case edgeTypes.right of
                        Exterior ->
                            let
                                lineSegment =
                                    LineSegment2d.fromEndpoints
                                        ( Point2d.translateBy
                                            extrusionVector
                                            endPoint
                                        , endPoint
                                        )
                            in
                            Just (LineSegment2dCurve lineSegment)

                        Interior ->
                            Nothing
            in
            List.filterMap identity
                [ startCurve
                , rightCurve
                , endCurve
                , leftCurve
                ]

        RevolutionRegion curve2d centerPoint sweptAngle edgeTypes ->
            let
                startCurve =
                    case edgeTypes.start of
                        Exterior ->
                            if sweptAngle >= 0.0 then
                                Just curve2d
                            else
                                Just (curve2dReverse curve2d)

                        Interior ->
                            Nothing

                endCurve =
                    case edgeTypes.end of
                        Exterior ->
                            let
                                rotated =
                                    curve2d
                                        |> curve2dRotateAround
                                            centerPoint
                                            sweptAngle
                            in
                            if sweptAngle >= 0.0 then
                                Just (curve2dReverse rotated)
                            else
                                Just rotated

                        Interior ->
                            Nothing

                insidePoint =
                    curve2dStartPoint curve2d

                outsidePoint =
                    curve2dEndPoint curve2d

                outsideCurve =
                    case edgeTypes.outside of
                        Exterior ->
                            let
                                arc =
                                    Arc2d.with
                                        { startPoint = outsidePoint
                                        , centerPoint = centerPoint
                                        , sweptAngle = sweptAngle
                                        }
                            in
                            if sweptAngle >= 0.0 then
                                Just (Arc2dCurve arc)
                            else
                                Just (Arc2dCurve (Arc2d.reverse arc))

                        Interior ->
                            Nothing

                insideCurve =
                    case edgeTypes.inside of
                        Exterior ->
                            let
                                arc =
                                    Arc2d.with
                                        { startPoint = insidePoint
                                        , centerPoint = centerPoint
                                        , sweptAngle = sweptAngle
                                        }
                            in
                            if sweptAngle >= 0.0 then
                                Just (Arc2dCurve (Arc2d.reverse arc))
                            else
                                Just (Arc2dCurve arc)

                        Interior ->
                            Nothing
            in
            List.filterMap identity
                [ startCurve
                , outsideCurve
                , endCurve
                , insideCurve
                ]

        FanRegion point curve2d edgeTypes ->
            let
                startCurve =
                    case edgeTypes.start of
                        Exterior ->
                            let
                                lineSegment =
                                    LineSegment2d.fromEndpoints
                                        ( point, curve2dStartPoint curve2d )
                            in
                            Just (LineSegment2dCurve lineSegment)

                        Interior ->
                            Nothing

                endCurve =
                    case edgeTypes.start of
                        Exterior ->
                            let
                                lineSegment =
                                    LineSegment2d.fromEndpoints
                                        ( curve2dEndPoint curve2d, point )
                            in
                            Just (LineSegment2dCurve lineSegment)

                        Interior ->
                            Nothing

                outsideCurve =
                    case edgeTypes.curve of
                        Exterior ->
                            Just curve2d

                        Interior ->
                            Nothing
            in
            List.filterMap identity
                [ startCurve
                , outsideCurve
                , endCurve
                ]

        Fused regions ->
            List.concat (List.map regionBoundaries regions)


regionToMesh : Float -> Region2d -> Mesh Point2d
regionToMesh tolerance region =
    case region of
        RectangleRegion rectangle _ ->
            let
                ( p0, p1, p2, p3 ) =
                    Rectangle2d.vertices rectangle
            in
            Mesh.with
                { vertices = Array.fromList [ p0, p1, p2, p3 ]
                , faceIndices =
                    if Frame2d.isRightHanded (Rectangle2d.axes rectangle) then
                        [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
                    else
                        [ ( 0, 2, 1 ), ( 0, 3, 2 ) ]
                }

        ExtrusionRegion curve2d extrusionVector _ ->
            let
                startVertices =
                    curve2dToPolyline tolerance curve2d
                        |> Polyline2d.vertices

                endVertices =
                    List.map (Point2d.translateBy extrusionVector) startVertices

                numColumns =
                    List.length startVertices - 1

                accumulate startVertices endVertices result =
                    case ( startVertices, endVertices ) of
                        ( startFirst :: startRest, endFirst :: endRest ) ->
                            accumulate
                                startRest
                                endRest
                                (startFirst :: endFirst :: result)

                        _ ->
                            result

                prependFaces columnIndex faces =
                    if columnIndex < numColumns then
                        let
                            offset =
                                2 * columnIndex

                            faceIndices1 =
                                ( offset, offset + 2, offset + 3 )

                            faceIndices2 =
                                ( offset, offset + 3, offset + 1 )
                        in
                        prependFaces (columnIndex + 1)
                            (faceIndices1 :: faceIndices2 :: faces)
                    else
                        faces
            in
            Mesh.with
                { vertices =
                    Array.fromList (accumulate startVertices endVertices [])
                , faceIndices = prependFaces 0 []
                }

        RevolutionRegion curve2d centerPoint sweptAngle _ ->
            let
                insidePoint =
                    curve2dStartPoint curve2d

                outsidePoint =
                    curve2dEndPoint curve2d

                outsideRadius =
                    Point2d.distanceFrom centerPoint outsidePoint

                numRotationSteps =
                    curveNumSegments tolerance
                        (outsideRadius * sweptAngle * sweptAngle)

                startCurve =
                    if sweptAngle >= 0.0 then
                        curve2d
                    else
                        curve2dReverse curve2d

                startVertices =
                    curve2dToPolyline tolerance startCurve
                        |> Polyline2d.vertices

                rotationAngles =
                    List.range 1 numRotationSteps
                        |> List.map
                            (\index ->
                                sweptAngle
                                    * (toFloat index / toFloat numRotationSteps)
                            )

                rotatedVertexLists =
                    rotationAngles
                        |> List.map
                            (\angle ->
                                startVertices
                                    |> List.map
                                        (Point2d.rotateAround centerPoint angle)
                            )

                numColumns =
                    List.length startVertices - 1

                numRows =
                    numRotationSteps

                prependFaces rowIndex columnIndex faces =
                    if columnIndex < numColumns then
                        let
                            i1 =
                                rowIndex * (numColumns + 1) + columnIndex

                            i2 =
                                i1 + 1

                            i4 =
                                i2 + numColumns

                            i3 =
                                i4 + 1

                            faceIndices1 =
                                ( i1, i2, i3 )

                            faceIndices2 =
                                ( i1, i3, i4 )
                        in
                        prependFaces
                            rowIndex
                            (columnIndex + 1)
                            (faceIndices1 :: faceIndices2 :: faces)
                    else if rowIndex < (numRows - 1) then
                        prependFaces (rowIndex + 1) 0 faces
                    else
                        faces
            in
            Mesh.with
                { vertices =
                    Array.fromList
                        (List.concat (startVertices :: rotatedVertexLists))
                , faceIndices = prependFaces 0 0 []
                }

        FanRegion point curve2d _ ->
            let
                curveVertices =
                    curve2dToPolyline tolerance curve2d |> Polyline2d.vertices

                numFaces =
                    List.length curveVertices - 1

                toFaceIndices faceIndex =
                    ( 0, faceIndex + 1, faceIndex + 2 )
            in
            Mesh.with
                { vertices = Array.fromList (point :: curveVertices)
                , faceIndices =
                    List.range 0 (numFaces - 1) |> List.map toFaceIndices
                }

        Fused regions ->
            Mesh.combine (List.map (regionToMesh tolerance) regions)


regionTranslateBy : Vector2d -> Region2d -> Region2d
regionTranslateBy displacement region =
    case region of
        RectangleRegion rectangle edgeTypes ->
            RectangleRegion
                (Rectangle2d.translateBy displacement rectangle)
                edgeTypes

        ExtrusionRegion curve2d extrusionVector edgeTypes ->
            ExtrusionRegion
                (curve2dTranslateBy displacement curve2d)
                extrusionVector
                edgeTypes

        RevolutionRegion curve2d centerPoint sweptAngle edgeTypes ->
            RevolutionRegion
                (curve2dTranslateBy displacement curve2d)
                (Point2d.translateBy displacement centerPoint)
                sweptAngle
                edgeTypes

        FanRegion point curve2d edgeTypes ->
            FanRegion
                (Point2d.translateBy displacement point)
                (curve2dTranslateBy displacement curve2d)
                edgeTypes

        Fused regions ->
            Fused (List.map (regionTranslateBy displacement) regions)


regionRotateAround : Point2d -> Float -> Region2d -> Region2d
regionRotateAround point angle region =
    case region of
        RectangleRegion rectangle edgeTypes ->
            RectangleRegion
                (Rectangle2d.rotateAround point angle rectangle)
                edgeTypes

        ExtrusionRegion curve2d extrusionVector edgeTypes ->
            ExtrusionRegion
                (curve2dRotateAround point angle curve2d)
                (Vector2d.rotateBy angle extrusionVector)
                edgeTypes

        RevolutionRegion curve2d centerPoint sweptAngle edgeTypes ->
            RevolutionRegion
                (curve2dRotateAround point angle curve2d)
                (Point2d.rotateAround point angle centerPoint)
                sweptAngle
                edgeTypes

        FanRegion fanPoint curve2d edgeTypes ->
            FanRegion
                (Point2d.rotateAround point angle fanPoint)
                (curve2dRotateAround point angle curve2d)
                edgeTypes

        Fused regions ->
            Fused (List.map (regionRotateAround point angle) regions)


regionMirrorAcross : Axis2d -> Region2d -> Region2d
regionMirrorAcross axis region =
    case region of
        RectangleRegion rectangle edgeTypes ->
            RectangleRegion
                (Rectangle2d.mirrorAcross axis rectangle)
                edgeTypes

        ExtrusionRegion curve2d extrusionVector edgeTypes ->
            ExtrusionRegion
                (curve2d |> curve2dMirrorAcross axis |> curve2dReverse)
                (Vector2d.mirrorAcross axis extrusionVector)
                edgeTypes

        RevolutionRegion curve2d centerPoint sweptAngle edgeTypes ->
            RevolutionRegion
                (curve2d |> curve2dMirrorAcross axis)
                (Point2d.mirrorAcross axis centerPoint)
                -sweptAngle
                edgeTypes

        FanRegion point curve2d edgeTypes ->
            FanRegion
                (Point2d.mirrorAcross axis point)
                (curve2d |> curve2dMirrorAcross axis |> curve2dReverse)
                edgeTypes

        Fused regions ->
            Fused (List.map (regionMirrorAcross axis) regions)


body3dExtrusion : Region2d -> SketchPlane3d -> Float -> Body3d
body3dExtrusion region sketchPlane distance =
    let
        planarSurface =
            surface3dPlanar region sketchPlane

        extrusionVector =
            Vector3d.with
                { length = distance
                , direction = SketchPlane3d.normalDirection sketchPlane
                }

        displacedSurface =
            surface3dTranslateBy extrusionVector planarSurface

        startSurface =
            if distance >= 0.0 then
                surface3dFlip planarSurface
            else
                planarSurface

        endSurface =
            if distance >= 0.0 then
                displacedSurface
            else
                surface3dFlip displacedSurface

        curves2d =
            if distance >= 0.0 then
                regionBoundaries region
            else
                regionBoundaries region |> List.map curve2dReverse

        sideSurfaces =
            curves2d
                |> List.map (curve3dOn sketchPlane)
                |> List.map (\curve -> surface3dExtrusion curve extrusionVector)
    in
    Body3d (startSurface :: endSurface :: sideSurfaces)


body3dSurfaces : Body3d -> List Surface3d
body3dSurfaces (Body3d surfaces) =
    surfaces


body3dTranslateBy : Vector3d -> Body3d -> Body3d
body3dTranslateBy displacement (Body3d surfaces) =
    Body3d (List.map (surface3dTranslateBy displacement) surfaces)


body3dRotateAround : Axis3d -> Float -> Body3d -> Body3d
body3dRotateAround axis angle (Body3d surfaces) =
    Body3d (List.map (surface3dRotateAround axis angle) surfaces)
