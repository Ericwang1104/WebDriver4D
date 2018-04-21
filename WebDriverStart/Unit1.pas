unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Webdriver4D,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.IniFiles,
  WD_httpDelphi, WD_http;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
		procedure FormDestroy(Sender: TObject);
		procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
		procedure Button3Click(Sender: TObject);
		procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
		procedure Memo1Change(Sender: TObject);
  private
    FCmd: TDriverCommand;
		FHProcess: Integer;
		FIni: TIniFile;
		FPM: TWebDriver;
    FStartupInfo: TStartupInfo;
    FProcessInfo: TProcessInformation;
		function GetAppPath: string;
		function GetPhantomjsFile: string;
  public
		property AppPath: string read GetAppPath;
		property PhantomjsFile: string read GetPhantomjsFile;
  end;

var
  Form1: TForm1;

implementation



{$R *.dfm}

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndnil(FCmd);
	FreeAndNil(FPM);
	FreeAndNil(FIni);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
	FIni :=TIniFile.Create(AppPath+'Set.ini');
  FPm :=TWebDriver.create(nil);
  FCmd :=TDelphiCommand.create(nil);
  Fpm.Cmd :=FCmd;
  FHProcess :=0;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  p: string;
begin
  FPM.CookieFiles :=AppPath +'Webdriver.cookies';
  fpm.DiskCache :=true;
  fpm.DiskCachePath :=AppPath+'cache';

  FPM.LogFile :=AppPath+'log.log';
  FPM.Address :='127.0.0.1';
  fpm.Port :=8080;
  FPM.StartPm(Memo1.Text);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
	fpm.TerminatePhantomjs;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
	FPM.Clear;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
	memo1.Lines.Text :=PhantomjsFile;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FProcessInfo.hProcess <> 0 then
  begin
    TerminateProcess(FProcessInfo.hProcess, 0);
    FHProcess :=0;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  Path:string;
begin
  Path :=GetAppPath;
  if FileExists(path+'phantomjs.exe') then
  begin
    Memo1.Text :=Path+'phantomjs.exe';
  end else
  begin
    Memo1.text :=self.PhantomjsFile;
  end;

end;

function TForm1.GetAppPath: string;
begin
	// TODO -cMM: TForm1.GetAppPath default body inserted
	Result :=ExtractFileDir(Application.ExeName) ;
  if  Result[Length(Result)]<>'\' then
  	result :=result+'\';
end;

function TForm1.GetPhantomjsFile: string;
begin
	// TODO -cMM: TForm1.GetPhantomjsFile default body inserted
	Result :=FIni.ReadString('SET','PhantomjsFile','') ;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
	FIni.WriteString('SET','PhantomjsFile',Memo1.Lines.Text);
end;

end.
