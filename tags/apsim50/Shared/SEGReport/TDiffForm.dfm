inherited DiffForm: TDiffForm
  Height = 557
  Caption = 'DiffForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited AdvPanelGroup1: TAdvPanelGroup
    Height = 523
    FullHeight = 18
    inherited AdvancedPanel: TAdvPanel
      FullHeight = 293
    end
    inherited PropertyPanel: TAdvPanel
      FullHeight = 297
      object Label3: TLabel
        Left = 13
        Top = 26
        Width = 67
        Height = 13
        Caption = 'Field name:'
      end
      object Label1: TLabel
        Left = 13
        Top = 82
        Width = 93
        Height = 13
        Caption = 'Second dataset:'
      end
      object FieldNameCombo: TComboBox
        Left = 13
        Top = 39
        Width = 189
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnChange = FieldNameComboChange
      end
      object SecondDataSetCombo: TComboBox
        Left = 13
        Top = 95
        Width = 189
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
        OnChange = SecondDataSetComboChange
      end
    end
  end
end
