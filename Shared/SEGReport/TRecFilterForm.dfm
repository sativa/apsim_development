inherited RecFilterForm: TRecFilterForm
  Left = 130
  Top = 174
  Caption = 'Filter form'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited AdvPanelGroup1: TAdvPanelGroup
    FullHeight = 18
    inherited AdvancedPanel: TAdvPanel
      FullHeight = 293
    end
    inherited PropertyPanel: TAdvPanel
      FullHeight = 297
      object RecLabel: TLabel
        Left = 21
        Top = 90
        Width = 93
        Height = 13
        Caption = 'Record number:'
      end
      object RecEdit: TEdit
        Left = 21
        Top = 110
        Width = 189
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        BevelKind = bkSoft
        BorderStyle = bsNone
        TabOrder = 0
        OnExit = RecEditExit
      end
      object FirstCheckBox: TCheckBox
        Left = 24
        Top = 40
        Width = 97
        Height = 17
        Caption = 'First record'
        TabOrder = 1
        OnClick = FirstCheckBoxClick
      end
      object LastCheckBox: TCheckBox
        Left = 24
        Top = 64
        Width = 97
        Height = 17
        Caption = 'Last record'
        TabOrder = 2
        OnClick = LastCheckBoxClick
      end
    end
  end
end
