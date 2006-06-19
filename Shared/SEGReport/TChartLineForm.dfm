inherited ChartLineForm: TChartLineForm
  Left = 197
  Top = 270
  Caption = 'ChartLineForm'
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
        Left = 25
        Top = 98
        Width = 47
        Height = 13
        Caption = 'Y value:'
      end
      object Label1: TLabel
        Left = 16
        Top = 48
        Width = 56
        Height = 13
        Caption = 'X1 Value:'
      end
      object Label4: TLabel
        Left = 16
        Top = 72
        Width = 56
        Height = 13
        Caption = 'X2 Value:'
      end
      object Label5: TLabel
        Left = 37
        Top = 120
        Width = 35
        Height = 13
        Caption = 'Label:'
      end
      object FieldNameCombo: TComboBox
        Left = 80
        Top = 94
        Width = 193
        Height = 21
        BevelKind = bkSoft
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnChange = FieldNameComboChange
      end
      object X1Edit: TEdit
        Left = 80
        Top = 48
        Width = 193
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = X1EditChange
      end
      object X2Edit: TEdit
        Left = 80
        Top = 72
        Width = 193
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = X2EditChange
      end
      object LabelEdit: TEdit
        Left = 80
        Top = 120
        Width = 193
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnExit = LabelEditExit
      end
    end
  end
end
