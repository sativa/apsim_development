inherited Frequency_form: TFrequency_form
  Left = 212
  Top = 98
  Caption = 'Frequency'
  PixelsPerInch = 96
  TextHeight = 16
  object Num_frequencies_label: TLabel [1]
    Left = 320
    Top = 360
    Width = 104
    Height = 16
    Caption = 'Num frequencies:'
  end
  inherited BitBtn2: TBitBtn
    TabOrder = 4
  end
  object GroupBox1: TGroupBox [7]
    Left = 336
    Top = 248
    Width = 177
    Height = 89
    Caption = 'Type of distribution:'
    TabOrder = 1
    object Frequency_radio: TRadioButton
      Left = 24
      Top = 24
      Width = 81
      Height = 17
      Caption = 'Frequency'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object Probability_radio: TRadioButton
      Left = 24
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Probability'
      TabOrder = 1
    end
  end
  object Num_frequencies_spin: TAdvSpinEdit [8]
    Left = 432
    Top = 355
    Width = 49
    Height = 26
    AutoFocus = False
    Direction = spVertical
    ReturnIsTab = False
    Precision = 0
    SpinType = sptNormal
    Value = 6
    FloatValue = 6
    HexValue = 0
    Flat = False
    FlatLineColor = clBlack
    SpinFlat = False
    SpinTransparent = False
    AutoSelect = False
    Color = clBtnFace
    EditorEnabled = False
    Enabled = True
    IncrementFloat = 0.1
    IncrementPage = 10
    IncrementFloatPage = 1
    IncrementSmart = False
    IncrementMinutes = 1
    IncrementHours = 1
    LabelAlwaysEnabled = False
    LabelPosition = lpLeftTop
    LabelMargin = 4
    LabelTransparent = False
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clWindowText
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    MaxValue = 10
    MinValue = 6
    MaxFloatValue = 100
    Signed = False
    ShowSeconds = True
    TabOrder = 3
    Visible = True
  end
  inherited ScenarioTree: TTreeView
    TabOrder = 5
  end
end
