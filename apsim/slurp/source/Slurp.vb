'Imports DotNetComponentInterface
Option Explicit On 
Imports VBGeneral
Imports System.Math
Imports VBMet
Imports CSGeneral.MathUtility
Imports ComponentInterface

Public Class Slurp
    Inherits ApsimComponent

    ' ----------------------  Component Constants-------------------------
    Private Const SVPfrac As Single = 0.66

    ' ------------------------  APSIM Public Data ---------------------------
    <ApsimVariable("crop_type", "", ApsimProperties.ReadWriteType.Read)> _
    Public CropType As String = ""   ' Type of plant simulated

    <ApsimVariable("int_radn", "MJ", ApsimProperties.ReadWriteType.Read)> _
    Public IntRadn As Single         ' Daily Intercepted Radiation (MJ/m2)

    <ApsimVariable("rlv", "mm/mm3", ApsimProperties.ReadWriteType.Read)> _
    Public rlv() As Single                 ' Root Length Density (mm/mm3)

    <ApsimVariable("sw_demand", "mm", ApsimProperties.ReadWriteType.Read), _
     ApsimVariable("pet", "mm", ApsimProperties.ReadWriteType.Read)> _
    Public SWDemand As Single           ' Daily Soil Water Demand (mm)

    ' ------------------------  Component Data ---------------------------
    Private LAI As Single                ' Leaf Area Index (Green)
    Private LAId As Single               ' Leaf Area Index (Dead)
    Private Kg As Single                 ' Extinction Coefficient (Green)
    Private Kd As Single                 ' Extinction Coefficient (Dead)
    Private FVPDFunction As New InterpSet  ' VPD effect on Growth Interpolation Set
    Private Height As Single             ' Canopy height (mm)
    Private Frgr As Single               ' Relative Growth Rate Factor
    Public dlayer() As Single         ' Soil Layer Thickness (mm)

    Private kl() As Single               ' SW uptake parameter (/day)
    Private ll() As Single               ' Crop Lower Limit (mm/mm)
    Private FtFunction As New InterpSet  ' Tmperature effect on Growth Interpolation Set
    Private Roots() As String            ' Array of root system names
    Private Root_Distance() As Single    ' Distance to each root system
    Private RootExtent As Single         ' Extent of root system (in multiples of height)
    Private SWSupply() As Single         ' Daily water supply from each layer (mm)
    Private SWUptake() As Single         ' Daily uptake of SW from each layer (mm)
    Private MetData As NewMet            ' Daily Met Data
    Private UptakeSource As String       ' User choice for source of uptake information

    ' ===================================================
    Public Overrides Sub init2()
        ReadParameters()             ' Get info from parameter data
        GetSoilData()                ' Get soil spec from water balance
        DoNewCropEvent()             ' Tell other modules that I exist
        DoNewCanopyEvent()           ' Tell other modules about my canopy
        DoNewPotentialGrowthEvent()  ' Tell other modules about my current growth status
    End Sub
    ' ===================================================
    Private Sub ReadParameters()

        ' Get UptakeSource - if missing set to "calc"
        UptakeSource = Data.ChildValue("uptake_source")
        If UptakeSource = "" Then UptakeSource = "calc"

        LAI = Convert.ToSingle(Data.Child("lai").Value)
        LAId = Convert.ToSingle(Data.Child("laid").Value)
        Kg = Convert.ToSingle(Data.Child("kg").Value)
        Kd = Convert.ToSingle(Data.Child("kd").Value)
        Height = Convert.ToSingle(Data.Child("height").Value)
        CropType = Data.Child("crop_type").Value
        Frgr = Convert.ToSingle(Data.Child("frgr").Value)

        FVPDFunction.data = Data.Child("fvpd")
        FtFunction.data = Data.Child("ft")


        Dim RootData As New RootParameters(Data.Child("layers"))
        rlv = RootData.rlv
        ll = RootData.ll
        kl = RootData.kl

    End Sub
    ' ===================================================
    Private Sub GetSoilData()
        properties.Get("dlayer", dlayer)
    End Sub

#Region "EventHandlers"
    <ApsimEvent("prepare")> Public Sub OnPrepare(ByVal Null As Null)
        DoNewPotentialGrowthEvent()
    End Sub
    <ApsimEvent("process")> Public Sub OnProcess(ByVal Null As Null)
        If UptakeSource = "calc" Then
            SWSupply = CalcSWSupply()
            SWUptake = CalcSWUptake()
            Dim DltSWDep(dlayer.Length - 1) As Single
            For layer As Integer = 0 To dlayer.Length - 1
                DltSWDep(layer) = SWUptake(layer) * -1
            Next
            properties.Set("dlt_sw_dep", DltSWDep)
        Else
            ' uptake is calculated by another module in APSIM
            properties.Get(Trim("uptake_water_" + CropType), SWUptake)
            'SWUptake = CalcSWSupply()
        End If
    End Sub
    <ApsimEvent("NewMet")> Public Sub OnNewMet(ByVal NewMetData As NewMet)
        MetData = NewMetData
    End Sub
    <ApsimEvent("canopy_water_balance")> Public Sub OnCanopyWaterBalance(ByVal CWB As CanopyWaterBalance)
        For i As Integer = 0 To CWB.Canopy.Count - 1
            If CWB.Canopy.value(i).name = Name Then
                ' It's me
                SWDemand = CWB.Canopy.value(i).PotentialEp
            End If
        Next
    End Sub
    Private Sub OnCanopyEnergyBalance() '(ByVal LP As LightProfile)
        'For j As Integer = 0 To LP.Interception.Count - 1
        'If LP.Interception(j).name = Me.Name Then
        'IntRadn = 0
        'LP.Interception.
        'End If
        'Next
    End Sub

#End Region
#Region "EventSenders"
    Private Sub DoNewCropEvent()
        ' Send out New Crop Event to tell other modules who I am and what I am
        Dim EventData As New NewCrop
        EventData.crop_type = CropType
        EventData.sender = Me.Name
        Events.Publish("new_crop", EventData)
    End Sub
    Private Sub DoNewCanopyEvent()
        Dim EventData As New NewCanopy
        EventData.sender = Me.Name
        EventData.lai = LAI
        EventData.lai_tot = LAI + LAId
        EventData.height = Height
        EventData.depth = Height
        EventData.cover = CoverGreen()
        EventData.cover_tot = CoverTot()

        events.Publish("new_canopy", EventData)
    End Sub
    Private Sub DoNewPotentialGrowthEvent()
        Dim EventData As New NewPotentialGrowth
        EventData.sender = Me.Name
        EventData.frgr = Min(Min(Frgr, Fvpd()), Ft())
        events.Publish("new_pot_growth", EventData)
    End Sub
#End Region
#Region "Properties"
    <ApsimProperty("LAI", "")> Public Property LAIproperty() As Single
        Get
            Return LAI
        End Get
        Set(ByVal Value As Single)
            LAI = Value
            DoNewCanopyEvent()
        End Set
    End Property
    <ApsimProperty("LAId", "")> Public Property LAIdproperty() As Single
        Get
            Return LAId
        End Get
        Set(ByVal Value As Single)
            LAId = Value
            DoNewCanopyEvent()
        End Set
    End Property
    <ApsimProperty("height", "mm")> Public Property Heightproperty() As Single
        Get
            Return Height
        End Get
        Set(ByVal Value As Single)
            Height = Value
            DoNewCanopyEvent()
        End Set
    End Property
    <ApsimProperty("frgr", "")> Public Property Frgrproperty() As Single
        Get
            Return Frgr
        End Get
        Set(ByVal Value As Single)
            Frgr = Value
            DoNewCanopyEvent()
        End Set
    End Property
    <ApsimProperty("cover_green", "")> Public ReadOnly Property CoverGreen() As Single
        Get
            Return 1.0 - Exp(-Kg * LAI)
        End Get
    End Property
    <ApsimProperty("cover_tot", "")> Public ReadOnly Property CoverTot() As Single
        Get
            Return 1.0 - (1 - CoverGreen()) * (1 - CoverDead())
        End Get
    End Property
    <ApsimProperty("cover_dead", "")> Public ReadOnly Property CoverDead() As Single
        Get
            Return 1.0 - Exp(-Kd * LAId)
        End Get
    End Property
    <ApsimProperty("Ft", "")> Public ReadOnly Property Ft() As Single
        Get
            Dim Tav As Single = (MetData.maxt + MetData.mint) / 2.0
            Return FtFunction.value(Tav)
        End Get
    End Property
    <ApsimProperty("Fvpd", "")> Public ReadOnly Property Fvpd() As Single
        Get
            Return FVPDFunction.value(VPD())
        End Get
    End Property
    <ApsimProperty("ep", "mm")> Public ReadOnly Property EP() As Single
        Get
            ' EP is daily total crop water use from all layers
            Dim ReturnValue As Single = 0
            For i As Integer = 0 To SWUptake.Length - 1
                ReturnValue = ReturnValue + SWUptake(i)
            Next
            Return ReturnValue
        End Get
    End Property

#End Region
#Region "Functions"
    ' The Following functions are used as utility functions or for calculating
    ' Values for output
    Private Function VPD() As Single

        Dim VPDmint As Single = svp(MetData.mint) - MetData.vp
        VPDmint = Max(VPDmint, 0.0)

        Dim VPDmaxt As Single = svp(MetData.maxt) - MetData.vp
        VPDmaxt = Max(VPDmaxt, 0.0)

        VPD = SVPfrac * VPDmaxt + (1 - SVPfrac) * VPDmint

    End Function

#End Region
#Region "Water Uptake"

    Private Function CalcSWSupply() As Single()
        Dim SWSupply(dlayer.Length - 1) As Single
        Dim SWdep() As Single
        Properties.Get("sw_dep", SWdep)

        For layer As Integer = 0 To dlayer.Length - 1
            SWSupply(layer) = Max(0.0, kl(layer) * (SWdep(layer) - ll(layer) * (dlayer(layer))))
        Next
        Return SWSupply
    End Function
    Private Function CalcSWUptake() As Single()
        Dim SWUptake(SWSupply.Length - 1) As Single
        Dim SWSupplyTot As Single = 0.0
        For layer As Integer = 0 To SWSupply.Length - 1
            SWSupplyTot = SWSupplyTot + SWSupply(layer)
        Next

        Dim Fraction As Single = Min(1.0, SWDemand / SWSupplyTot)
        For layer As Integer = 0 To SWSupply.Length - 1
            SWUptake(layer) = SWSupply(layer) * Fraction
        Next
        Return SWUptake
    End Function
#End Region
    Private Class InterpSet
        Private XVals() As Double
        Private YVals() As Double

        Public WriteOnly Property data() As APSIMData
            Set(ByVal Points As APSIMData)
                ReDim XVals(Points.Children.Count - 1)
                ReDim YVals(Points.Children.Count - 1)
                Dim i As Integer = -1
                For Each point As APSIMData In Points.Children
                    i = i + 1
                    XVals(i) = Convert.ToSingle(point.Child("x").Value)
                    YVals(i) = Convert.ToSingle(point.Child("y").Value)
                Next

            End Set
        End Property
        Public Function value(ByVal x As Single) As Single
            Dim flag As Boolean
            value = LinearInterpReal(CType(x, Single), XVals, YVals, flag)
        End Function
    End Class

    Private Class RootParameters

        Private MyData As APSIMData
        Public Sub New(ByVal Data As APSIMData)
            MyData = Data
        End Sub


        Private Function value(ByVal name As String) As Single()

            Dim ReturnValue(MyData.Children.Count - 1) As Single

            Dim i As Integer = -1
            For Each layer As APSIMData In MyData.Children
                i = i + 1
                If Not IsNothing(layer.Child(name)) Then
                    ReturnValue(i) = layer.Child(name).Value
                End If
            Next

            Return ReturnValue
        End Function
        Public ReadOnly Property rlv() As Single()
            Get
                Return value("rlv")
            End Get
        End Property
        Public ReadOnly Property ll() As Single()
            Get
                Return value("ll")
            End Get
        End Property
        Public ReadOnly Property kl() As Single()
            Get
                Return value("kl")
            End Get
        End Property
    End Class

End Class
