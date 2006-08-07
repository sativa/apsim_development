inherited PredObsForm: TPredObsForm
  Left = 75
  Top = 135
  Height = 557
  Caption = 'PredObsForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited AdvPanelGroup1: TAdvPanelGroup
    Height = 522
    FullHeight = 18
    inherited AdvancedPanel: TAdvPanel
      FullHeight = 293
    end
    inherited PropertyPanel: TAdvPanel
      FullHeight = 297
      object Label3: TLabel
        Left = 13
        Top = 138
        Width = 107
        Height = 13
        Caption = 'Key field name(s):'
      end
      object Label1: TLabel
        Left = 13
        Top = 34
        Width = 104
        Height = 13
        Caption = 'Predicted dataset:'
      end
      object Label4: TLabel
        Left = 13
        Top = 82
        Width = 106
        Height = 13
        Caption = 'Observed dataset:'
      end
      object PredCombo: TComboBox
        Left = 13
        Top = 47
        Width = 260
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnChange = PredComboChange
      end
      object FieldNameList: TListBox
        Left = 16
        Top = 160
        Width = 257
        Height = 161
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 1
        OnClick = FieldNameListClick
      end
      object ObsCombo: TComboBox
        Left = 16
        Top = 96
        Width = 260
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 2
        OnChange = ObsComboChange
      end
    end
  end
end
