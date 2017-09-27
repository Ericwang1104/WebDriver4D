object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'PhantomJs Server'
  ClientHeight = 283
  ClientWidth = 564
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
    Left = 48
    Top = 57
    Width = 142
    Height = 49
    Caption = #21551#21160'PhantomJs'
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
    Left = 216
    Top = 57
    Width = 153
    Height = 49
    Caption = #32456#27490'Phantomjs'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 408
    Top = 57
    Width = 137
    Height = 49
    Caption = #28165#31354'Session'
    TabOrder = 3
    OnClick = Button3Click
  end
end
