unit frm_WebDrvier_Service;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Webdriver4D,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.IniFiles,
  WD_httpDelphi, WD_http, Vcl.ExtCtrls;

type
  TTWebDriverService = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    rgBrowserType: TRadioGroup;
    Button4: TButton;
		procedure FormDestroy(Sender: TObject);
		procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
		procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
		procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
		procedure Memo1Change(Sender: TObject);
  private
    FCmd: TDriverCommand;
		FHProcess: Integer;
		FIni: TIniFile;
    FWD: TWebDriver;
    FStartupInfo: TStartupInfo;
    FProcessInfo: TProcessInformation;
		function GetAppPath: string;
    function GetWebDriverFile: string;
  public
		property AppPath: string read GetAppPath;
    property WebDriverFile: string read GetWebDriverFile;
  end;

var
  TWebDriverService: TTWebDriverService;

implementation



{$R *.dfm}

procedure TTWebDriverService.FormDestroy(Sender: TObject);
begin
  if Assigned(FCmd) then
    FreeAndnil(FCmd);
  if Assigned(FWD) then
	  FreeAndNil(FWD);
	FreeAndNil(FIni);
end;

procedure TTWebDriverService.FormCreate(Sender: TObject);
begin
	FIni :=TIniFile.Create(AppPath+'Set.ini');
  FWD :=TWebDriver.create(nil);
  FCmd :=TDelphiCommand.create(nil);
  FWD.Cmd :=FCmd;
  FHProcess :=0;
end;

procedure TTWebDriverService.Button1Click(Sender: TObject);
var
  p: string;
begin
  if Assigned(FWD) then FreeAndNil(FWD);
  case rgBrowserType.ItemIndex of
    0:  //IE
    begin
      FWD :=TIEDriver.Create(nil);
      FWD.Cmd :=FCmd;
      FWD.StartDriver('..\..\..\WebDriver\IEDriverServer_x86.exe');
    end;
    1:  //firefox
    begin
      FWD :=TFireFoxDriver.Create(nil);
      FWD.Cmd :=FCmd;
      (FWD as TFireFoxDriver).BrowserFileName :='C:\Program Files\Mozilla Firefox\firefox.exe';
      FWD.StartDriver('..\..\..\WebDriver\Geckodriver_x86.exe');
    end;
    2://chrome
    begin
      FWD :=TChromeDriver.Create(nil);
      FWD.Cmd :=FCMD;
      FWD.StartDriver('..\..\..\WebDriver\chromedriver.exe');
    end;
    3:  //phantomjs
    begin
      FWd :=TPhantomjs.Create(nil);
      FWd.Cmd :=FCMd;
      fwd.StartDriver('..\..\..\WebDriver\phantomjs.exe');
    end;

  end;
end;

procedure TTWebDriverService.Button2Click(Sender: TObject);
begin
	FWD.TerminateWebDriver;
end;

procedure TTWebDriverService.Button3Click(Sender: TObject);
begin
	FWD.Clear;
end;

procedure TTWebDriverService.Button4Click(Sender: TObject);
begin
  FWD.NewSession;
end;

procedure TTWebDriverService.FormActivate(Sender: TObject);
begin
	memo1.Lines.Text :=WebDriverFile;
end;

procedure TTWebDriverService.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FProcessInfo.hProcess <> 0 then
  begin
    TerminateProcess(FProcessInfo.hProcess, 0);
    FHProcess :=0;
  end;
end;

procedure TTWebDriverService.FormShow(Sender: TObject);
var
  Path:string;
begin
  Path :=GetAppPath;
  if FileExists(path+'phantomjs.exe') then
  begin
    Memo1.Text :=Path+'phantomjs.exe';
  end else
  begin
    Memo1.text :=self.WebDriverFile;
  end;

end;

function TTWebDriverService.GetAppPath: string;
begin
	// TODO -cMM: TForm1.GetAppPath default body inserted
	Result :=ExtractFileDir(Application.ExeName) ;
  if  Result[Length(Result)]<>'\' then
  	result :=result+'\';
end;

function TTWebDriverService.GetWebDriverFile: string;
begin
	// TODO -cMM: TForm1.GetWebDriverFile default body inserted
	Result :=FIni.ReadString('SET','WebDriver','') ;
end;

procedure TTWebDriverService.Memo1Change(Sender: TObject);
begin
	FIni.WriteString('SET','webdriver',Memo1.Lines.Text);
end;

end.
