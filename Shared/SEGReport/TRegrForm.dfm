inherited RegrForm: TRegrForm
  Left = 72
  Top = 156
  Caption = 'RegrForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited AdvPanelGroup1: TAdvPanelGroup
    FullHeight = 18
    inherited AdvancedPanel: TAdvPanel
      FullHeight = 310
      inherited NameEdit: TEdit
        Width = 102
      end
      inherited SourceCombo: TComboBox
        Width = 102
      end
      inherited SortFieldsEdit: TEdit
        Width = 132
      end
      inherited GroupByEdit: TEdit
        Width = 132
      end
    end
    inherited PropertyPanel: TAdvPanel
      FullHeight = 38
      object Label3: TLabel
        Left = 13
        Top = 26
        Width = 79
        Height = 13
        Caption = 'X Field name:'
      end
      object Label1: TLabel
        Left = 13
        Top = 74
        Width = 78
        Height = 13
        Caption = 'Y Field name:'
      end
      object XCombo: TComboBox
        Left = 13
        Top = 46
        Width = 141
        Height = 21
        BevelKind = bkSoft
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnChange = XComboChange
      end
    end
  end
  object YCombo: TComboBox
    Left = 21
    Top = 102
    Width = 141
    Height = 21
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
    OnChange = YComboChange
  end
end
