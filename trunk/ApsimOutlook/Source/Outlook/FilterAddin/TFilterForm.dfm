object FilterForm: TFilterForm
  Left = 247
  Top = 156
  Width = 546
  Height = 268
  BorderIcons = []
  Caption = 'Filter Values'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn2: TBitBtn
    Left = 216
    Top = 200
    Width = 75
    Height = 25
    TabOrder = 0
    Kind = bkCancel
  end
  object BitBtn1: TBitBtn
    Left = 136
    Top = 200
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 538
    Height = 193
    ActivePage = Scenario1Sheet
    Align = alTop
    TabIndex = 0
    TabOrder = 2
    object Scenario1Sheet: TTabSheet
      Caption = 'Scenario1'
      object FilterBox1: TPSCFltBox
        Left = 0
        Top = 32
        Width = 524
        Height = 129
        SQLDateTimeFormat.AlwaysY2k = False
        OnChange = FilterBox1Change
        TabOrder = 0
        AdvancedFilter = False
        FilterOptions = []
        DefaultTemplates = False
        DataSet = MemTable1
        Templates = <
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'contains'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValuePrefix = '%'
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'begins with'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'ends with'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValuePrefix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
            UsageID = 1
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'doesn'#39't contain'
            Filter = 'NOT (:Field LIKE :Value)'
            UsageID = 1
            ValuePrefix = '%'
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'more than'
            Filter = ':Field > :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'less than'
            Filter = ':Field < :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'at least'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'at most'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'after'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'before'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~yesterday~'
            Filter = '((:Field >= :SYSYESTERDAY) and (:Field < :SYSTODAY))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~today~'
            Filter = '((:Field >= :SYSTODAY) and (:Field < :SYSTOMORROW))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~tomorrow~'
            Filter = '((:Field >= :SYSTOMORROW) and (:Field < :SYSTODP2))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~last 7 days~'
            Filter = '((:Field < :SYSTODAY) and (:Field >= :SYSTODM7))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~next 7 days~'
            Filter = '((:Field >= :SYSTOMORROW) and (:Field < :SYSTODP8))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on the ~last week~'
            Filter = '((:Field < :SYSTODAY) and (:Field >= :SYSLastWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on ~this week~'
            Filter = '((:Field < :SYSNextWeek1) and (:Field >= :SYSThisWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on the ~next week~'
            Filter = '((:Field < :SYSNextWeek8) and (:Field >= :SYSNextWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in ~last month~'
            Filter = '((:Field < :SYSThisMon1) and (:Field >= :SYSLastMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in ~this month~'
            Filter = '((:Field < :SYSNextMon1) and (:Field >= :SYSThisMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~next month~'
            Filter = '((:Field < :SYSNextMon32) and (:Field >= :SYSNextMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on date~'
            Filter = '((:Field >= DATE(:Value)) and (:Field < (DATE(:Value)+1)))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on~'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on or after~'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on or before~'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Unknown'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Unknown'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template :Value'
            CaptionID = 'equals'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template :Value'
            CaptionID = 'not equal to'
            Filter = ':Field <> :Value'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'equals'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'not equal to'
            Filter = ':Field <> :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'at most'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'at least'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'more than'
            Filter = ':Field > :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'less than'
            Filter = ':Field < :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end>
      end
      object FilterCombo1: TComboBox
        Left = 0
        Top = 8
        Width = 524
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = FilterCombo1Change
      end
    end
    object Scenario2Sheet: TTabSheet
      Caption = 'Scenario2'
      ImageIndex = 1
      object FilterCombo2: TComboBox
        Left = 0
        Top = 8
        Width = 524
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = FilterCombo2Change
      end
      object FilterBox2: TPSCFltBox
        Left = 0
        Top = 32
        Width = 524
        Height = 129
        SQLDateTimeFormat.AlwaysY2k = False
        OnChange = FilterBox2Change
        TabOrder = 1
        AdvancedFilter = False
        FilterOptions = []
        DefaultTemplates = False
        DataSet = MemTable2
        Templates = <
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'contains'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValuePrefix = '%'
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'begins with'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'ends with'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValuePrefix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
            UsageID = 1
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'doesn'#39't contain'
            Filter = 'NOT (:Field LIKE :Value)'
            UsageID = 1
            ValuePrefix = '%'
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'more than'
            Filter = ':Field > :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'less than'
            Filter = ':Field < :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'at least'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'at most'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'after'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'before'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~yesterday~'
            Filter = '((:Field >= :SYSYESTERDAY) and (:Field < :SYSTODAY))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~today~'
            Filter = '((:Field >= :SYSTODAY) and (:Field < :SYSTOMORROW))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~tomorrow~'
            Filter = '((:Field >= :SYSTOMORROW) and (:Field < :SYSTODP2))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~last 7 days~'
            Filter = '((:Field < :SYSTODAY) and (:Field >= :SYSTODM7))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~next 7 days~'
            Filter = '((:Field >= :SYSTOMORROW) and (:Field < :SYSTODP8))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on the ~last week~'
            Filter = '((:Field < :SYSTODAY) and (:Field >= :SYSLastWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on ~this week~'
            Filter = '((:Field < :SYSNextWeek1) and (:Field >= :SYSThisWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on the ~next week~'
            Filter = '((:Field < :SYSNextWeek8) and (:Field >= :SYSNextWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in ~last month~'
            Filter = '((:Field < :SYSThisMon1) and (:Field >= :SYSLastMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in ~this month~'
            Filter = '((:Field < :SYSNextMon1) and (:Field >= :SYSThisMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~next month~'
            Filter = '((:Field < :SYSNextMon32) and (:Field >= :SYSNextMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on date~'
            Filter = '((:Field >= DATE(:Value)) and (:Field < (DATE(:Value)+1)))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on~'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on or after~'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on or before~'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Unknown'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Unknown'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template :Value'
            CaptionID = 'equals'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template :Value'
            CaptionID = 'not equal to'
            Filter = ':Field <> :Value'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'equals'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'not equal to'
            Filter = ':Field <> :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'at most'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'at least'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'more than'
            Filter = ':Field > :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'less than'
            Filter = ':Field < :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end>
      end
    end
    object Scenario3Sheet: TTabSheet
      Caption = 'Scenario3'
      ImageIndex = 2
      object FilterCombo3: TComboBox
        Left = 0
        Top = 8
        Width = 524
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = FilterCombo3Change
      end
      object FilterBox3: TPSCFltBox
        Left = 0
        Top = 32
        Width = 524
        Height = 129
        SQLDateTimeFormat.AlwaysY2k = False
        OnChange = FilterBox3Change
        TabOrder = 1
        AdvancedFilter = False
        FilterOptions = []
        DefaultTemplates = False
        DataSet = MemTable3
        Templates = <
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'contains'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValuePrefix = '%'
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'begins with'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'ends with'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValuePrefix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
            UsageID = 1
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'doesn'#39't contain'
            Filter = 'NOT (:Field LIKE :Value)'
            UsageID = 1
            ValuePrefix = '%'
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'more than'
            Filter = ':Field > :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'less than'
            Filter = ':Field < :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'at least'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'at most'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'after'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'before'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~yesterday~'
            Filter = '((:Field >= :SYSYESTERDAY) and (:Field < :SYSTODAY))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~today~'
            Filter = '((:Field >= :SYSTODAY) and (:Field < :SYSTOMORROW))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~tomorrow~'
            Filter = '((:Field >= :SYSTOMORROW) and (:Field < :SYSTODP2))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~last 7 days~'
            Filter = '((:Field < :SYSTODAY) and (:Field >= :SYSTODM7))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~next 7 days~'
            Filter = '((:Field >= :SYSTOMORROW) and (:Field < :SYSTODP8))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on the ~last week~'
            Filter = '((:Field < :SYSTODAY) and (:Field >= :SYSLastWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on ~this week~'
            Filter = '((:Field < :SYSNextWeek1) and (:Field >= :SYSThisWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on the ~next week~'
            Filter = '((:Field < :SYSNextWeek8) and (:Field >= :SYSNextWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in ~last month~'
            Filter = '((:Field < :SYSThisMon1) and (:Field >= :SYSLastMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in ~this month~'
            Filter = '((:Field < :SYSNextMon1) and (:Field >= :SYSThisMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~next month~'
            Filter = '((:Field < :SYSNextMon32) and (:Field >= :SYSNextMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on date~'
            Filter = '((:Field >= DATE(:Value)) and (:Field < (DATE(:Value)+1)))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on~'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on or after~'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on or before~'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Unknown'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Unknown'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template :Value'
            CaptionID = 'equals'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template :Value'
            CaptionID = 'not equal to'
            Filter = ':Field <> :Value'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'equals'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'not equal to'
            Filter = ':Field <> :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'at most'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'at least'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'more than'
            Filter = ':Field > :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'less than'
            Filter = ':Field < :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end>
      end
    end
    object Scenario4Sheet: TTabSheet
      Caption = 'Scenario4'
      ImageIndex = 3
      object FilterCombo4: TComboBox
        Left = 0
        Top = 8
        Width = 524
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = FilterCombo4Change
      end
      object FilterBox4: TPSCFltBox
        Left = 0
        Top = 32
        Width = 524
        Height = 129
        SQLDateTimeFormat.AlwaysY2k = False
        OnChange = FilterBox4Change
        TabOrder = 1
        AdvancedFilter = False
        FilterOptions = []
        DefaultTemplates = False
        DataSet = MemTable4
        Templates = <
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'contains'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValuePrefix = '%'
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'begins with'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'ends with'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValuePrefix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
            UsageID = 1
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'doesn'#39't contain'
            Filter = 'NOT (:Field LIKE :Value)'
            UsageID = 1
            ValuePrefix = '%'
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'more than'
            Filter = ':Field > :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'less than'
            Filter = ':Field < :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'at least'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'at most'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'after'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'before'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~yesterday~'
            Filter = '((:Field >= :SYSYESTERDAY) and (:Field < :SYSTODAY))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~today~'
            Filter = '((:Field >= :SYSTODAY) and (:Field < :SYSTOMORROW))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~tomorrow~'
            Filter = '((:Field >= :SYSTOMORROW) and (:Field < :SYSTODP2))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~last 7 days~'
            Filter = '((:Field < :SYSTODAY) and (:Field >= :SYSTODM7))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~next 7 days~'
            Filter = '((:Field >= :SYSTOMORROW) and (:Field < :SYSTODP8))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on the ~last week~'
            Filter = '((:Field < :SYSTODAY) and (:Field >= :SYSLastWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on ~this week~'
            Filter = '((:Field < :SYSNextWeek1) and (:Field >= :SYSThisWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on the ~next week~'
            Filter = '((:Field < :SYSNextWeek8) and (:Field >= :SYSNextWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in ~last month~'
            Filter = '((:Field < :SYSThisMon1) and (:Field >= :SYSLastMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in ~this month~'
            Filter = '((:Field < :SYSNextMon1) and (:Field >= :SYSThisMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~next month~'
            Filter = '((:Field < :SYSNextMon32) and (:Field >= :SYSNextMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on date~'
            Filter = '((:Field >= DATE(:Value)) and (:Field < (DATE(:Value)+1)))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on~'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on or after~'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on or before~'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Unknown'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Unknown'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template :Value'
            CaptionID = 'equals'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template :Value'
            CaptionID = 'not equal to'
            Filter = ':Field <> :Value'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'equals'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'not equal to'
            Filter = ':Field <> :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'at most'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'at least'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'more than'
            Filter = ':Field > :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'less than'
            Filter = ':Field < :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end>
      end
    end
    object Scenario5Sheet: TTabSheet
      Caption = 'Scenario5'
      ImageIndex = 4
      object FilterCombo5: TComboBox
        Left = 0
        Top = 8
        Width = 524
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = FilterCombo5Change
      end
      object FilterBox5: TPSCFltBox
        Left = 0
        Top = 32
        Width = 524
        Height = 129
        SQLDateTimeFormat.AlwaysY2k = False
        OnChange = FilterBox5Change
        TabOrder = 1
        AdvancedFilter = False
        FilterOptions = []
        DefaultTemplates = False
        DataSet = MemTable5
        Templates = <
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'contains'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValuePrefix = '%'
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'begins with'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'ends with'
            Filter = ':Field LIKE :Value'
            UsageID = 1
            ValuePrefix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
            UsageID = 1
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template :Value'
            CaptionID = 'doesn'#39't contain'
            Filter = 'NOT (:Field LIKE :Value)'
            UsageID = 1
            ValuePrefix = '%'
            ValueSuffix = '%'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Memo'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'more than'
            Filter = ':Field > :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'less than'
            Filter = ':Field < :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'at least'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Text'
            Caption = ':Field :Template :Value'
            CaptionID = 'at most'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'is (exactly)'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'after'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value'
            CaptionID = 'before'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Time'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~yesterday~'
            Filter = '((:Field >= :SYSYESTERDAY) and (:Field < :SYSTODAY))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~today~'
            Filter = '((:Field >= :SYSTODAY) and (:Field < :SYSTOMORROW))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is ~tomorrow~'
            Filter = '((:Field >= :SYSTOMORROW) and (:Field < :SYSTODP2))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~last 7 days~'
            Filter = '((:Field < :SYSTODAY) and (:Field >= :SYSTODM7))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~next 7 days~'
            Filter = '((:Field >= :SYSTOMORROW) and (:Field < :SYSTODP8))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on the ~last week~'
            Filter = '((:Field < :SYSTODAY) and (:Field >= :SYSLastWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on ~this week~'
            Filter = '((:Field < :SYSNextWeek1) and (:Field >= :SYSThisWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is on the ~next week~'
            Filter = '((:Field < :SYSNextWeek8) and (:Field >= :SYSNextWeek1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in ~last month~'
            Filter = '((:Field < :SYSThisMon1) and (:Field >= :SYSLastMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in ~this month~'
            Filter = '((:Field < :SYSNextMon1) and (:Field >= :SYSThisMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is in the ~next month~'
            Filter = '((:Field < :SYSNextMon32) and (:Field >= :SYSNextMon1))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on date~'
            Filter = '((:Field >= DATE(:Value)) and (:Field < (DATE(:Value)+1)))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on~'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on or after~'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value'
            CaptionID = 'is ~on or before~'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Date'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Unknown'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Unknown'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template :Value'
            CaptionID = 'equals'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template :Value'
            CaptionID = 'not equal to'
            Filter = ':Field <> :Value'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template'
            CaptionID = 'is empty'
            Filter = ':Field IS NULL'
          end
          item
            Category = 'Boolean'
            Caption = ':Field :Template'
            CaptionID = 'is not empty'
            Filter = ':Field <> NULL'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'equals'
            Filter = ':Field = :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'not equal to'
            Filter = ':Field <> :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'at most'
            Filter = ':Field <= :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'at least'
            Filter = ':Field >= :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'more than'
            Filter = ':Field > :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value'
            CaptionID = 'less than'
            Filter = ':Field < :Value'
          end
          item
            Category = 'Number'
            Caption = ':Field :Template :Value1 and :Value2'
            CaptionID = 'between'
            Filter = '((:Field >= :Value1) and (:Field <= :Value2))'
          end>
      end
    end
  end
  object TurnOffButton: TBitBtn
    Left = 296
    Top = 200
    Width = 113
    Height = 25
    Caption = '&Turn filter off'
    ModalResult = 1
    TabOrder = 3
    OnClick = TurnOffButtonClick
    NumGlyphs = 2
  end
  object MemTable1: TkbmMemTable
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <>
    IndexDefs = <>
    SortOptions = []
    AllDataOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveBlobs, mtfSaveFiltered, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail, mtfSaveDeltas]
    CommaTextOptions = [mtfSaveData]
    CSVQuote = '"'
    CSVFieldDelimiter = ','
    CSVRecordDelimiter = ','
    CSVTrueString = 'True'
    CSVFalseString = 'False'
    PersistentSaveOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail]
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    FilterOptions = []
    Version = '2.53g'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    Left = 216
    Top = 64
  end
  object MemTable2: TkbmMemTable
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <>
    IndexDefs = <>
    SortOptions = []
    AllDataOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveBlobs, mtfSaveFiltered, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail, mtfSaveDeltas]
    CommaTextOptions = [mtfSaveData]
    CSVQuote = '"'
    CSVFieldDelimiter = ','
    CSVRecordDelimiter = ','
    CSVTrueString = 'True'
    CSVFalseString = 'False'
    PersistentSaveOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail]
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    FilterOptions = []
    Version = '2.53g'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    Left = 248
    Top = 64
  end
  object MemTable3: TkbmMemTable
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <>
    IndexDefs = <>
    SortOptions = []
    AllDataOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveBlobs, mtfSaveFiltered, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail, mtfSaveDeltas]
    CommaTextOptions = [mtfSaveData]
    CSVQuote = '"'
    CSVFieldDelimiter = ','
    CSVRecordDelimiter = ','
    CSVTrueString = 'True'
    CSVFalseString = 'False'
    PersistentSaveOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail]
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    FilterOptions = []
    Version = '2.53g'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    Left = 280
    Top = 64
  end
  object MemTable4: TkbmMemTable
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <>
    IndexDefs = <>
    SortOptions = []
    AllDataOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveBlobs, mtfSaveFiltered, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail, mtfSaveDeltas]
    CommaTextOptions = [mtfSaveData]
    CSVQuote = '"'
    CSVFieldDelimiter = ','
    CSVRecordDelimiter = ','
    CSVTrueString = 'True'
    CSVFalseString = 'False'
    PersistentSaveOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail]
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    FilterOptions = []
    Version = '2.53g'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    Left = 312
    Top = 64
  end
  object MemTable5: TkbmMemTable
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <>
    IndexDefs = <>
    SortOptions = []
    AllDataOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveBlobs, mtfSaveFiltered, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail, mtfSaveDeltas]
    CommaTextOptions = [mtfSaveData]
    CSVQuote = '"'
    CSVFieldDelimiter = ','
    CSVRecordDelimiter = ','
    CSVTrueString = 'True'
    CSVFalseString = 'False'
    PersistentSaveOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail]
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    FilterOptions = []
    Version = '2.53g'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    Left = 348
    Top = 64
  end
end
