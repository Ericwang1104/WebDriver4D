unit frm_main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Webdriver4D,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, System.IniFiles, WD_httpDelphi;

type
  TForm1 = class(TForm)
    rgWebDriver: TRadioGroup;
    memLog: TMemo;
    Label1: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    txtGetURL: TEdit;
    Label2: TLabel;
    Button1: TButton;
    ActionList1: TActionList;
    actCmdGetURL: TAction;
    Label3: TLabel;
    txtWebDriverPath: TEdit;
    Action1: TAction;
    actStartWebDriver: TAction;
    Button2: TButton;
    Label4: TLabel;
    txtSession: TEdit;
    txtFindName: TEdit;
    actFindElementByTag: TAction;
    Button3: TButton;
    txtElement: TEdit;
    actSwitchFrame: TAction;
    actGentInnerHTML: TAction;
    Button4: TButton;
    procedure actCmdGetURLExecute(Sender: TObject);
    procedure actFindElementByTagExecute(Sender: TObject);
    procedure actGentInnerHTMLExecute(Sender: TObject);
    procedure actStartWebDriverExecute(Sender: TObject);
    procedure actSwitchFrameExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function CreateWebDriver: TWebDriver;
    procedure txtGetURLChange(Sender: TObject);
    procedure txtWebDriverPathChange(Sender: TObject);
  private
    FCMD: TDelphiCommand;
    FcurElement: TWebElement;
    FIni: TIniFile;
    FWD: TWebDriver;
    function GetAppPath: string;
    function GetWebdriverPath: string;
    procedure SetWebdriverPath(const Value: string);
    { Private declarations }
  public
    property AppPath: string read GetAppPath;
    property WebdriverPath: string read GetWebdriverPath write SetWebdriverPath;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.WideStrUtils, System.StrUtils;

{$R *.dfm}

procedure TForm1.actCmdGetURLExecute(Sender: TObject);
begin
  FWD.GetURL(txtGetURL.Text);
end;

procedure TForm1.actFindElementByTagExecute(Sender: TObject);

begin
  FcurElement := FWd.FindElementByTag(txtFindName.Text);
  memLog.Lines.Add(FcurElement.Text);
end;

procedure TForm1.actGentInnerHTMLExecute(Sender: TObject);
var
  Element:TWebElement;
begin
  Element :=FcurElement;
  memLog.Lines.Add(FcurElement.PropertyValue('innerHTML'));
end;

procedure TForm1.actStartWebDriverExecute(Sender: TObject);
begin
  FWD :=CreateWebDriver;
end;

procedure TForm1.actSwitchFrameExecute(Sender: TObject);
begin
  FWD.SwitchToFrame(txtFindName.Text);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FWD) then FreeAndNil(FWD);
  FreeAndNil(FCMD);
  FreeAndNil(FIni);
end;

procedure TForm1.FormCreate(Sender: TObject);

begin
  FIni :=TIniFile.Create(AppPath+'Set.ini');
  FCMD :=TDelphiCommand.Create(self);
  WebdriverPath :=FIni.ReadString('Path','webdriverpath',AppPath+'webdriver\');
  txtGetURL.Text :=FIni.ReadString('Value','URL','')
end;

function TForm1.CreateWebDriver: TWebDriver;
var
  WD:TWebDriver;
  IE:TIEDriver absolute WD;
  FFox:TFireFoxDriver absolute WD;
  Chrome:TChromeDriver absolute WD;
  Edge:TEdgeDriver absolute WD;
  Phantomjs:TPhantomjs absolute WD;
begin
  if Assigned(FWD) then FreeAndNil(FWD);

  case rgWebDriver.ItemIndex of
    0:  //IE
    begin
      IE :=TIEDriver.Create(nil);
      IE.StartDriver(WebdriverPath+'IEDriverServer_X86.exe');
      IE.Cmd :=FCMD;
      txtSession.Text :=IE.NewSession;
      result :=IE;
    end;
    1: //FireFox
    begin
      FFox := TFireFoxDriver.Create(nil);
      FFox.BrowserFileName :='C:\Program Files\Mozilla Firefox\firefox.exe';
      FFox.StartDriver(WebdriverPath+'geckodriver_x86.exe');
      FFox.Cmd :=FCMD;
      txtSession.Text :=FFox.NewSession;
      result :=FFox;
    end;
    2: //chrome
    begin
      Chrome :=TChromeDriver.Create(nil);
      Chrome.StartDriver(WebdriverPath+'chromedriver.exe');
      txtSession.Text :=Chrome.NewSession;
      result :=Chrome;
    end;
    3: //Edge
    begin
      Edge :=TEdgeDriver.Create(nil);
      Edge.StartDriver(WebdriverPath+'MicrosoftWebDriver.exe');
      Edge.Cmd :=FCMD;
      txtSession.Text :=Edge.NewSession;
      result :=Edge;
    end;
    4: //Phantomjs
    begin
      Phantomjs :=TPhantomjs.Create(nil);
      Phantomjs.StartDriver(WebdriverPath+'Phantomjs.exe');
      Phantomjs.Cmd :=FCMD;
      txtSession.Text :=Phantomjs.NewSession;
      result :=Phantomjs;
    end;

  end;
end;

function TForm1.GetAppPath: string;
begin
  // TODO -cMM: TForm1.GetAppPath default body inserted
  Result :=ExtractFilePath(Application.ExeName) ;
end;

function TForm1.GetWebdriverPath: string;
begin
  if RightStr(txtWebDriverPath.Text,1)<>'\' then
    Result :=txtWebDriverPath.Text+'\'
  else
    Result :=txtWebDriverPath.Text;
end;

procedure TForm1.SetWebdriverPath(const Value: string);
begin
  txtWebDriverPath.Text :=Value;

end;

procedure TForm1.txtGetURLChange(Sender: TObject);
begin
  FIni.WriteString('Value','URL',txtGetURL.Text);
end;

procedure TForm1.txtWebDriverPathChange(Sender: TObject);
begin
  FIni.WriteString('Path','webdriverpath',WebdriverPath);
end;

end.
