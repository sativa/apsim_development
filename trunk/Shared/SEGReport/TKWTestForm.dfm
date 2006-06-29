inherited KWTestForm: TKWTestForm
  Left = 164
  Top = 173
  Caption = 'KWTestForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited AdvPanelGroup1: TAdvPanelGroup
    FullHeight = 18
    inherited AdvancedPanel: TAdvPanel
      Height = 25
      FullHeight = 310
      inherited NameEdit: TEdit
        Width = 242
      end
      inherited SourceCombo: TComboBox
        Width = 242
      end
      inherited SortFieldsEdit: TEdit
        Width = 116
      end
      inherited GroupByEdit: TEdit
        Width = 116
      end
    end
    inherited PropertyPanel: TAdvPanel
      FullHeight = 38
      object Label3: TLabel
        Left = 13
        Top = 82
        Width = 67
        Height = 13
        Caption = 'Field name:'
      end
      object Label1: TLabel
        Left = 16
        Top = 40
        Width = 72
        Height = 13
        Caption = '2nd dataset:'
      end
      object FieldNameCombo: TComboBox
        Left = 13
        Top = 102
        Width = 260
        Height = 21
        BevelKind = bkSoft
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnChange = FieldNameComboChange
      end
      object Source2Combo: TComboBox
        Left = 13
        Top = 54
        Width = 260
        Height = 21
        BevelKind = bkSoft
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
        OnChange = Source2ComboChange
      end
    end
  end
end
