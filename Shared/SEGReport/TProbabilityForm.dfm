inherited ProbabilityForm: TProbabilityForm
  Caption = 'ProbabilityForm'
  OldCreateOrder = True
  PixelsPerInch = 120
  TextHeight = 16
  inherited AdvPanelGroup1: TAdvPanelGroup
    FullHeight = 18
    inherited AdvancedPanel: TAdvPanel
      FullHeight = 293
      inherited NameEdit: TEdit
        Width = 174
      end
      inherited SourceCombo: TComboBox
        Width = 174
      end
    end
    inherited PropertyPanel: TAdvPanel
      FullHeight = 297
      object Label3: TLabel
        Left = 16
        Top = 32
        Width = 76
        Height = 16
        Caption = 'Field name:'
      end
      object FieldNameCombo: TComboBox
        Left = 16
        Top = 56
        Width = 174
        Height = 24
        BevelKind = bkSoft
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        TabOrder = 0
        OnChange = FieldNameComboChange
      end
      object ExceedenceCheckBox: TCheckBox
        Left = 17
        Top = 96
        Width = 232
        Height = 21
        Caption = 'Probability of exceedance?'
        TabOrder = 1
        OnClick = ExceedenceCheckBoxClick
      end
    end
  end
end
