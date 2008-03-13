Option Explicit On
Imports VBGeneral
Imports System.Math
Imports VBMet
Imports CSGeneral
Imports ModelFramework

<Model()> Public Class Slurp

    Public ScienceAPI As ScienceAPI

    ' ----------------------  Component Constants-------------------------
    Private Const SVPfrac As Single = 0.66

    ' ------------------------  APSIM Public Data ---------------------------
    <Output()> _
    Public Crop_Type As String = ""   ' Type of plant simulated

    <Output()> <Units("MJ")> _
    Public IntRadn As Single         ' Daily Intercepted Radiation (MJ/m2)

    <Output()> <Units("mm/mm3")> _
    Public rlv() As Single                 ' Root Length Density (mm/mm3)

    Public SWDemand As Single           ' Daily Soil Water Demand (mm)

    ' ------------------------  Component Data ---------------------------
    Private _LAI As Single                ' Leaf Area Index (Green)
    Private _LAId As Single               ' Leaf Area Index (Dead)
    Private Kg As Single                 ' Extinction Coefficient (Green)
    Private Kd As Single                 ' Extinction Coefficient (Dead)
    Private FVPDFunction As New InterpSet  ' VPD effect on Growth Interpolation Set
    Private _Height As Single             ' Canopy height (mm)
    Private _Frgr As Single               ' Relative Growth Rate Factor
    Public dlayer() As Single        ' Soil Layer Thickness (mm)

    Private kl() As Single               ' SW uptake parameter (/day)
    Private ll() As Single               ' Crop Lower Limit (mm/mm)
    Private FtFunction As New InterpSet  ' Tmperature effect on Growth Interpolation Set
    Private Roots() As String            ' Array of root system names
    Private Root_Distance() As Single    ' Distance to each root system
    Private RootExtent As Single         ' Extent of root system (in multiples of height)
    Private SWSupply() As Single         ' Daily water supply from each layer (mm)
    Private SWUptake() As Double         ' Daily uptake of SW from each layer (mm)
    Private MetData As New NewMetType    ' Daily Met Data
    Private UptakeSource As String       ' User choice for source of uptake information
    Private Zones() As String
    Private Distances() As Single

    Public Sub New(ByVal api As ScienceAPI)
        ScienceAPI = api
    End Sub

    ' ===================================================
    <EventHandler()> Public Sub OnInit2()
        ReadParameters()             ' Get info from parameter data
        GetSoilData()                ' Get soil spec from water balance
        DoNewCropEvent()             ' Tell other modules that I exist
        DoNewCanopyEvent()           ' Tell other modules about my canopy
        DoNewPotentialGrowthEvent()  ' Tell other modules about my current growth status
    End Sub
    ' ===================================================
    Private Sub ReadParameters()

        ' Get UptakeSource - if missing set to "calc"
        If Not ScienceAPI.Read("uptake_source", "", True, UptakeSource) Then
            UptakeSource = "calc"
        End If

        If UptakeSource = "distributed" Then
            ScienceAPI.Read("zones", "", False, Zones)
            ScienceAPI.Read("distances", "", False, Distances)
        End If

        ScienceAPI.Read("lai", "", False, _LAI)
        ScienceAPI.Read("laid", "", False, _LAId)
        ScienceAPI.Read("kg", "", False, Kg)
        ScienceAPI.Read("kd", "", False, Kd)
        ScienceAPI.Read("height", "", False, _Height)
        ScienceAPI.Read("crop_type", "", False, Crop_Type)
        ScienceAPI.Read("frgr", "", False, _Frgr)
        ScienceAPI.Read("fvpd", "", False, FVPDFunction)
        ScienceAPI.Read("ft", "", False, FtFunction)

        ScienceAPI.Read("rlv", "", False, rlv)
        ScienceAPI.Read("ll", "", False, ll)
        ScienceAPI.Read("kl", "", False, kl)

        'Dim RootData As New RootParameters(Data.Child("layers"))
        'rlv = RootData.rlv
        'll = RootData.ll
        'kl = RootData.kl

    End Sub
    ' ===================================================
    Private Sub GetSoilData()
        If UptakeSource = "calc" Then
            ScienceAPI.Get("dlayer", "()", dlayer)
            If dlayer.Length <> ll.Length Then
                Throw New Exception("Number of values of LL does not match the number of soil layers.")
            End If
            If dlayer.Length <> kl.Length Then
                Throw New Exception("Number of values of KL does not match the number of soil layers.")
            End If
            If dlayer.Length <> rlv.Length Then
                Throw New Exception("Number of values of RLV does not match the number of soil layers.")
            End If
        End If
    End Sub

#Region "EventHandlers"
    <EventHandler()> _
    Public Sub OnPrepare()
        DoNewPotentialGrowthEvent()
    End Sub
    <EventHandler()> _
    Public Sub OnProcess()
        If UptakeSource = "calc" Then
            SWSupply = CalcSWSupply()
            SWUptake = CalcSWUptake()
            Dim DltSWDep(dlayer.Length - 1) As Single
            For layer As Integer = 0 To dlayer.Length - 1
                DltSWDep(layer) = SWUptake(layer) * -1
            Next
            ScienceAPI.Set("dlt_sw_dep", "mm", DltSWDep)

        ElseIf UptakeSource = "distributed" Then
            Dim Supply(Zones.Length - 1) As Single
            Dim SupplyTot As Single = 0
            Dim WeightingTot As Single = 0
            Dim Weighting(Zones.Length - 1) As Single
            Dim AdjSupply(Zones.Length - 1) As Single
            Dim AdjSupplyTot As Single = 0
            For i As Integer = 0 To Zones.Length - 1
                ScienceAPI.Get(Zones(i) + ".root_sw_supply", "()", Supply(i))
                'MsgBox(SupplyTot(i), MsgBoxStyle.Information, "Paddock Es")
                Weighting(i) = (1.0 - bound(Distances(i) / (3 * Height / 1000.0), 0.0, 1.0)) ^ 1
                Supply(i) = Supply(i)
                AdjSupply(i) = Supply(i) * Weighting(i)
                If Weighting(i) > 0 Then SupplyTot = SupplyTot + Supply(i)
                AdjSupplyTot = AdjSupplyTot + AdjSupply(i)
                WeightingTot = WeightingTot + Weighting(i)
            Next
            If SupplyTot > 0 Then
                If SWDemand > SupplyTot Then
                    ' Get as much as can be supplied
                    For i As Integer = 0 To Zones.Length - 1
                        If Weighting(i) > 0 And Supply(i) > 0.01 Then
                            ScienceAPI.Set(Zones(i) + ".sw_demand", "mm", Supply(i))
                        End If
                    Next
                Else
                    ' Scale back demands based upon source strength and distance
                    Dim Fraction = SWDemand / AdjSupplyTot
                    For i As Integer = 0 To Zones.Length - 1
                        Supply(i) = AdjSupply(i) * Fraction
                        ScienceAPI.Set(Zones(i) + ".sw_demand", "mm", Supply(i))
                    Next
                End If
            Else
                For i As Integer = 0 To Zones.Length - 1
                    Dim zilch As Single = 0
                    ScienceAPI.Set(Zones(i) + ".sw_demand", "mm", zilch)
                Next
            End If

        Else
            ' uptake is calculated by another module in APSIM
            'Dim SWUptake(1) As Single
            ScienceAPI.Get(Trim("uptake_water_" + Crop_Type), "()", SWUptake)
            'SWUptake = CalcSWSupply()
        End If
    End Sub
    <EventHandler()> _
    Public Sub OnNewMet(ByVal NewMetData As NewMetType)
        MetData = NewMetData
    End Sub
    <EventHandler()> _
    Public Sub OnCanopy_Water_Balance(ByVal CWB As CanopyWaterBalanceType)
        For i As Integer = 0 To CWB.Canopy.Length - 1
            If CWB.Canopy(i).name = ScienceAPI.Name() Then
                ' It's me
                SWDemand = CWB.Canopy(i).PotentialEp
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
        Dim EventData As New NewCropType
        EventData.crop_type = Crop_Type
        EventData.sender = ScienceAPI.Name()
        ScienceAPI.Publish("NewCrop", EventData)
    End Sub
    Private Sub DoNewCanopyEvent()
        Dim EventData As New NewCanopyType
        EventData.sender = ScienceAPI.Name()
        EventData.lai = LAI
        EventData.lai_tot = LAI + LAId
        EventData.height = Height
        EventData.depth = Height
        EventData.cover = Cover_green()
        EventData.cover_tot = Cover_tot()

        ScienceAPI.Publish("new_canopy", EventData)
    End Sub
    Private Sub DoNewPotentialGrowthEvent()
        Dim EventData As New NewPotentialGrowthType
        EventData.sender = ScienceAPI.Name()
        EventData.frgr = Min(Min(Frgr, Fvpd()), Ft())
        ScienceAPI.Publish("newpotentialgrowth", EventData)
    End Sub
#End Region
#Region "Properties"
    <Output()> Public Property LAI() As Single
        Get
            Return _LAI
        End Get
        Set(ByVal Value As Single)
            _LAI = Value
            DoNewCanopyEvent()
        End Set
    End Property
    <Output()> Public Property LAId() As Single
        Get
            Return _LAId
        End Get
        Set(ByVal Value As Single)
            _LAId = Value
            DoNewCanopyEvent()
        End Set
    End Property
    <Output()> <Units("mm")> Public Property Height() As Single
        Get
            Return _Height
        End Get
        Set(ByVal Value As Single)
            _Height = Value
            DoNewCanopyEvent()
        End Set
    End Property
    <Output()> Public Property Frgr() As Single
        Get
            Return _Frgr
        End Get
        Set(ByVal Value As Single)
            _Frgr = Value
            DoNewCanopyEvent()
        End Set
    End Property
    <Output()> <Units("")> Public ReadOnly Property Cover_green() As Single
        Get
            Return 1.0 - Exp(-Kg * LAI)
        End Get
    End Property
    <Output()> <Units("")> Public ReadOnly Property Cover_tot() As Single
        Get
            Return 1.0 - (1 - Cover_green()) * (1 - Cover_dead())
        End Get
    End Property
    <Output()> <Units("")> Public ReadOnly Property Cover_dead() As Single
        Get
            Return 1.0 - Exp(-Kd * LAId)
        End Get
    End Property
    <Output()> Public ReadOnly Property Ft() As Single
        Get
            Dim Tav As Single = (MetData.maxt + MetData.mint) / 2.0
            Return FtFunction.value(Tav)
        End Get
    End Property
    <Output()> Public ReadOnly Property Fvpd() As Single
        Get
            Return FVPDFunction.value(VPD())
        End Get
    End Property
    <Output()> <Units("mm")> Public ReadOnly Property EP() As Single
        Get
            If UptakeSource = "distributed" Then
                ' EP is daily total crop water use from all zones
                Dim ReturnValue As Single = 0
                Dim temp As Single = 0
                For i As Integer = 0 To Zones.Length - 1
                    ScienceAPI.Get(Zones(i) + ".root_ep", "()", temp)
                    ReturnValue = ReturnValue + temp
                Next
                Return ReturnValue

            ElseIf Not IsNothing(SWUptake) Then
                ' EP is daily total crop water use from all layers
                Dim ReturnValue As Single = 0
                For i As Integer = 0 To SWUptake.Length - 1
                    ReturnValue = ReturnValue + SWUptake(i)
                Next
                Return ReturnValue

            End If
            Return 0
        End Get
    End Property
    <Output()> <Units("mm")> Public ReadOnly Property Root_EP() As Single
        Get
            If UptakeSource = "distributed" Then
                ' EP is daily total crop water use from all zones
                Dim ReturnValue As Single = 0
                Dim temp As Single = 0
                For i As Integer = 0 To Zones.Length - 1
                    ScienceAPI.Get(Zones(i) + ".root_ep", "()", temp)
                    ReturnValue = ReturnValue + temp
                Next
                Return ReturnValue

            ElseIf Not IsNothing(SWUptake) Then
                ' EP is daily total crop water use from all layers
                Dim ReturnValue As Single = 0
                For i As Integer = 0 To SWUptake.Length - 1
                    ReturnValue = ReturnValue + SWUptake(i)
                Next
                Return ReturnValue

            End If
            Return 0
        End Get
    End Property

    <Output()> <Units("mm")> Public Property SW_Demand() As Single
        Get
            Return SWDemand
        End Get
        Set(ByVal value As Single)
            SWDemand = value
        End Set
    End Property
    <Output()> <Units("mm")> Public ReadOnly Property root_sw_supply() As Single
        Get
            If Not IsNothing(dlayer) Then
                Dim SWSupplyTot As Single
                Dim Supply As Single() = CalcSWSupply()
                For layer As Integer = 0 To Supply.Length - 1
                    SWSupplyTot = SWSupplyTot + Supply(layer)
                Next
                Return SWSupplyTot
            End If
            Return 0
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
        ScienceAPI.Get("sw_dep", "()", SWdep)
        For layer As Integer = 0 To dlayer.Length - 1
            SWSupply(layer) = Max(0.0, kl(layer) * (SWdep(layer) - ll(layer) * (dlayer(layer))))
        Next
        Return SWSupply
    End Function
    Private Function CalcSWUptake() As Double()
        Dim SWUptake(SWSupply.Length - 1) As Double
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
#Region "Utilities"
    Public Function StringToSingleArray(ByVal Stream As String) As Single()
        Dim stringSeparators() As String = {" "}

        Dim ValueStrings As String() = Stream.Split(stringSeparators, StringSplitOptions.RemoveEmptyEntries)
        Dim ReturnValue(ValueStrings.Length - 1) As Single

        For i As Integer = 0 To ValueStrings.Length - 1
            ReturnValue(i) = Convert.ToSingle(ValueStrings(i))
        Next

        Return ReturnValue
    End Function
    Public Function StringToStringArray(ByVal Stream As String) As String()
        Dim stringSeparators() As String = {" "}

        Dim ValueStrings As String() = Stream.Split(stringSeparators, StringSplitOptions.RemoveEmptyEntries)
        Return ValueStrings

    End Function

#End Region
    Private Class InterpSet
        Private XVals() As Double
        Private YVals() As Double

        Public Function value(ByVal x As Single) As Single
            Dim flag As Boolean
            value = MathUtility.LinearInterpReal(CType(x, Single), XVals, YVals, flag)
        End Function

        Public Shared Widening Operator CType(ByVal d As InterpSet) As String
            Return ""
        End Operator
        Public Shared Narrowing Operator CType(ByVal Xml As String) As InterpSet
            Dim Table As New Xml.XmlDocument()
            Table.LoadXml(Xml)
            Dim NumPoints As Integer = XmlHelper.ChildNodes(Table.DocumentElement, "").Count
            Dim Result As New InterpSet
            ReDim Result.XVals(NumPoints - 1)
            ReDim Result.YVals(NumPoints - 1)
            Dim i As Integer = -1
            For Each point As Xml.XmlNode In XmlHelper.ChildNodes(Table.DocumentElement, "")
                i = i + 1
                Result.XVals(i) = Convert.ToSingle(XmlHelper.Value(point, "x"))
                Result.YVals(i) = Convert.ToSingle(XmlHelper.Value(point, "y"))
            Next
            Return Result
        End Operator
    End Class

    Private Class RootParameters

        Private MyData As Xml.XmlNode
        Public Sub New(ByVal Data As Xml.XmlNode)
            MyData = Data
        End Sub


        Private Function value(ByVal name As String) As Single()

            Dim ReturnValue(XmlHelper.ChildNodes(MyData, "").Count - 1) As Single

            Dim i As Integer = -1
            For Each layer As Xml.XmlNode In XmlHelper.ChildNodes(MyData, "")
                i = i + 1
                If Not IsNothing(XmlHelper.Find(layer, name)) Then
                    ReturnValue(i) = XmlHelper.Value(layer, name)
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
