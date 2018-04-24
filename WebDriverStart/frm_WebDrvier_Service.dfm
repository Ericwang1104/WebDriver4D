object TWebDriverService: TTWebDriverService
  Left = 0
  Top = 0
  Caption = 'WebDriver-service'
  ClientHeight = 331
  ClientWidth = 718
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 40
    Top = 201
    Width = 142
    Height = 49
    Caption = 'start service'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 32
    Top = 8
    Width = 513
    Height = 25
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
    OnChange = Memo1Change
  end
  object Button2: TButton
    Left = 232
    Top = 201
    Width = 153
    Height = 49
    Caption = #32456#27490'Phantomjs'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 432
    Top = 201
    Width = 137
    Height = 49
    Caption = #28165#31354'Session'
    TabOrder = 3
    OnClick = Button3Click
  end
  object rgBrowserType: TRadioGroup
    Left = 32
    Top = 56
    Width = 513
    Height = 105
    Caption = 'BrowserType'
    Columns = 4
    ItemIndex = 0
    Items.Strings = (
      'Ie driver'
      'firefox driver'
      'chrome driver'
      'phantomjs driver')
    TabOrder = 4
  end
  object Button4: TButton
    Left = 40
    Top = 256
    Width = 142
    Height = 49
    Caption = 'New Session'
    TabOrder = 5
    OnClick = Button4Click
  end
end
