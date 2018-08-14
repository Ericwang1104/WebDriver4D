object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 554
  ClientWidth = 757
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    757
    554)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 407
    Width = 34
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Result:'
    ExplicitTop = 369
  end
  object Label3: TLabel
    Left = 8
    Top = 25
    Width = 75
    Height = 13
    Caption = 'Webdriver Path'
  end
  object Label4: TLabel
    Left = 8
    Top = 125
    Width = 36
    Height = 13
    Caption = 'Session'
  end
  object rgWebDriver: TRadioGroup
    Left = 8
    Top = 49
    Width = 741
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = 'WebDriver'
    Columns = 5
    ItemIndex = 0
    Items.Strings = (
      'IE Driver'
      'FireFox Driver'
      'Chrome Driver'
      'Edge Driver'
      'Phantomjs Driver')
    TabOrder = 0
  end
  object memLog: TMemo
    Left = 8
    Top = 426
    Width = 741
    Height = 111
    Anchors = [akLeft, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 144
    Width = 741
    Height = 257
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'command1'
      object Label2: TLabel
        Left = 3
        Top = 24
        Width = 19
        Height = 13
        Caption = 'URL'
      end
      object txtGetURL: TEdit
        Left = 28
        Top = 21
        Width = 265
        Height = 21
        TabOrder = 0
        OnChange = txtGetURLChange
      end
      object Button1: TButton
        Left = 312
        Top = 28
        Width = 105
        Height = 25
        Action = actCmdGetURL
        TabOrder = 1
      end
      object txtFindName: TEdit
        Left = 28
        Top = 64
        Width = 265
        Height = 21
        TabOrder = 2
        Text = 'txtFindName'
      end
      object Button3: TButton
        Left = 312
        Top = 59
        Width = 105
        Height = 25
        Action = actFindElementByTag
        TabOrder = 3
      end
      object txtElement: TEdit
        Left = 28
        Top = 104
        Width = 121
        Height = 21
        TabOrder = 4
        Text = 'txtElement'
      end
      object Button4: TButton
        Left = 423
        Top = 28
        Width = 105
        Height = 25
        Action = actGentInnerHTML
        TabOrder = 5
      end
    end
  end
  object txtWebDriverPath: TEdit
    Left = 89
    Top = 22
    Width = 553
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = txtWebDriverPathChange
  end
  object Button2: TButton
    Left = 648
    Top = 18
    Width = 101
    Height = 25
    Action = actStartWebDriver
    Anchors = [akTop, akRight]
    TabOrder = 4
  end
  object txtSession: TEdit
    Left = 50
    Top = 117
    Width = 699
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clAppWorkSpace
    ReadOnly = True
    TabOrder = 5
    OnChange = txtWebDriverPathChange
  end
  object ActionList1: TActionList
    Left = 568
    Top = 184
    object actCmdGetURL: TAction
      Caption = 'GetURL'
      OnExecute = actCmdGetURLExecute
    end
    object Action1: TAction
      Caption = 'Action1'
    end
    object actStartWebDriver: TAction
      Caption = 'Start WebDriver'
      OnExecute = actStartWebDriverExecute
    end
    object actFindElementByTag: TAction
      Caption = 'FindElementBy_Tag'
      OnExecute = actFindElementByTagExecute
    end
    object actSwitchFrame: TAction
      Caption = 'SwitchFrame'
      OnExecute = actSwitchFrameExecute
    end
    object actGentInnerHTML: TAction
      Caption = 'GentInnerHTML'
      OnExecute = actGentInnerHTMLExecute
    end
  end
end
