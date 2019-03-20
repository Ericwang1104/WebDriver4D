unit Webdriver4D;

interface

uses
  Classes, SysUtils, Windows, Contnrs, Vcl.Graphics, WD_http,
  {$IFDEF FPC} {$ELSE} WD_httpDelphi, {$ENDIF}
//{$IFDEF FPC} {$ELSE} WD_httpCis, {$ENDIF}
  JsonDataObjects, Winapi.ShlObj;

type
  TWebDriverClass=class of TWebDriver;
  TCommandType = (cGet, cPost, cDelete);
  TDriverType = (btPhantomjs, btIE, btFirefox, btChrome,btEdge);
  TWebDriver = class;

  TWebElement = packed record
  private
    FKeyName: string;
    FUsingName: string;
    FWebDriver: TWebDriver;
    function GetValue: string;
    function GetTagName: string;
    function GetSize: string;
    function GetText: string;
  public
    W3C: Boolean;
    ElementData: string;
    function AttributeValue(aName: string): string;
    function PropertyValue(aName: string): string;
    procedure Clear;
    procedure Click;
    function IsEmpty: Boolean;
    function Location: string;
    procedure ScreenShot(const FileName: string); overload;
    procedure ScreenShot(var bmp: TBitmap); overload;
    procedure ScreenShot2(var bmp: TBitmap); overload; // 有的浏览器不支持
    procedure SendKey(Key: string);

    property WebDriver: TWebDriver read FWebDriver write FWebDriver;
    property UsingName: string read FUsingName write FUsingName;
    property KeyName: string read FKeyName write FKeyName;
    property Value: string read GetValue; // 元件ID值
    property TagName: string read GetTagName;
    property Size: string read GetSize;
    property Text: string read GetText;
  end;

  TWebElements = packed record
  private
    FKeyName: string;
    FUsingName: string;
    FWebDriver: TWebDriver;
    function GetCount: Integer;
    function GetItems(Index: Integer): TWebElement;
  public
    W3C: Boolean;
    ElementsData: String;
    property KeyName: string read FKeyName write FKeyName;
    property UsingName: string read FUsingName write FUsingName;
    property WebDriver: TWebDriver read FWebDriver write FWebDriver;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TWebElement read GetItems;
  end;

  TExecCommandEvent = procedure(response: string) of object;


  TWebDriver = class(TComponent)
  private
    FAddress: string;
    FCmd: TDriverCommand;
    FErrorMessage: string;
    FOnResponse: TExecCommandEvent;
    FPopup_Error: Boolean;
    FProcessInfo: TProcessInformation;
    FStartupInfo: TStartupInfo;
    function GetDriverIsRunning: Boolean;
    function GetHasError: Boolean;
    function GetHost: string;
    function GetTimeout: Integer;
    procedure SetTimeout(const Value: Integer);
  strict protected
    FDriverName: string;
    FLogFile: string;
    FPath: string;
    FPort: Integer;
    FSessionID: string;
    FW3C: Boolean;
    function BuildParams: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ExecuteCommand(const CommandType: TCommandType;
      const Command: string; const Param: string = ''): string;
    function ProcResponse(const Resp: string): string;
    procedure CutImage(Pic: string; X, Y, Width, Height: Integer;
      var bmp: TBitmap);
    procedure Base64ToBmp(Pic: string; var bmp: TBitmap);
    procedure BmpToPng(bmp: TBitmap; const FileName: string);
    procedure StartDriver(const ExeName: string;
      const Args: string = ''); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure CloseWindow(ParamSessionID: string = '');
    function GetAllCookie: string;
    function GetAllCookieJsonArray: string;
    function GetCookieByName(cookieName: string): string;
    function AddCookie(cookieName, cookieValue: string): string;

    procedure DeleteAllCookie;
    procedure DeleteCookie(const cookieName: string);

    function FindElement(UsingName, KeyName: string): TWebElement;
    function FindElementByID(const ID: string): TWebElement;
    function FindElementByTag(const TagName: string): TWebElement;
    function FindElementByClassName(const ClasName: string): TWebElement;
    function FindElementByLinkText(const LinkText: string): TWebElement;
    function FindElementByXPath(XPath: string): TWebElement;

    function FindElements(const UsingName, KeyName: string): TWebElements;
    function FindElementsByXPath(XPath: string): TWebElements;
    function FindElementsByTag(const TagName: string): TWebElements;
    function FindElementsByLinkText(const LinkText: string): TWebElements;
    function FindElementsByID(const ID: string): TWebElements;
    function FindElementsByClassName(const ClasName: string): TWebElements;

    function GetCurrentWindowHandle: string;
    procedure GetURL(const URL: string);
    function GetCurUrl: string;
    function NewSession: string; virtual; abstract;
    procedure DeleteSession(ParamSessionID: string = '');
    function GetDocument: string;
    function GetAllSession: string;
    procedure Set_Window_Size(const Width, Height: Integer;
      WindowHandle: string = 'current');
    function ExecuteScript(const Script: string; const Args: string = '[]')
      : string; virtual;
    procedure ExecuteScriptByASync(const Script: string;
      const Args: string = '[]'); virtual;
    procedure Implicitly_Wait(const waitTime: Double);
    procedure PageLoadTimeout(const Timeout: Integer);
    procedure Quit;
    procedure Refresh(ParamSessionID: string = '');
    procedure SaveCurDocToFile(const FileName: string);
    procedure ScreenShot(var bmp: TBitmap); overload;
    procedure ScreenShot(const FileName: string); overload;

    procedure SwitchToFrame(const FrameID: string); virtual;
    procedure SwitchToParentFrame; virtual;
    procedure TerminateWebDriver;
    procedure WaitForLoaded;
    property ErrorMessage: string read FErrorMessage;
    property HasError: Boolean read GetHasError;
    property LogFile: string read FLogFile write FLogFile;
    property Host: string read GetHost;
    property Address: string read FAddress write FAddress;
    property Cmd: TDriverCommand read FCmd write FCmd;
    property DriverIsRunning: Boolean read GetDriverIsRunning;
    property DriverName: string read FDriverName;
    property Port: Integer read FPort write FPort;
    property Path: string read FPath write FPath;
    property Popup_Error: Boolean read FPopup_Error write FPopup_Error;
    property SessionID: string read FSessionID write FSessionID;
    property W3C: Boolean read FW3C;
  published
    procedure Clear;
    property Timeout: Integer read GetTimeout write SetTimeout;
    property OnResponse: TExecCommandEvent read FOnResponse write FOnResponse;
  end;

  TPhantomjs = class(TWebDriver)
  private
    FCookieFiles: string;
    FDiskCache: Boolean;
    FDiskCachePath: string;
  strict protected
    function BuildParams: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    function ExecuteScript(const Script: string; const Args: string = '[]')
      : string; override;
    function Execute_Phantom_Script(const Script: string; const Args: string =
        '[]'): string;
    function NewSession: string; override;
    property CookieFiles: string read FCookieFiles write FCookieFiles;
    property DiskCache: Boolean read FDiskCache write FDiskCache;
    property DiskCachePath: string read FDiskCachePath write FDiskCachePath;
  end;

  TIEDriver = class(TWebDriver)
  strict protected
    function BuildParams: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function NewSession: string; override;
  end;

  TFireFoxDriver = class(TWebDriver)
  private
    FBrowserFileName: string;
    FnewVersion: Boolean;
  strict protected
    function BuildParams: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function NewSession: string; override;
    procedure StartDriver(const ExeName: string;
      const Args: string = ''); override;
    property BrowserFileName: string read FBrowserFileName
      write FBrowserFileName;
    property newVersion: Boolean read FnewVersion write FnewVersion;
  end;

  TChromeDriver = class(TWebDriver)
  strict protected
    function BuildParams: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function NewSession: string; override;
    procedure StartDriver(const ExeName: string;
      const Args: string = ''); override;
  end;

  TEdgeDriver = class(TWebDriver)
  strict protected
    function BuildParams: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function NewSession: string; override;
    procedure SwitchToFrame(const FrameID: string); override;
  end;

implementation

uses
  System.NetEncoding, Vcl.Imaging.pngimage, System.StrUtils, Winapi.TlHelp32,
  System.Variants;

constructor TWebDriver.Create(AOwner: TComponent);
begin
  inherited;
  FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
  FAddress := '127.0.0.1';
  FLogFile := '';
  FPath := '';

  FErrorMessage := '';
  FW3C := False;
  FPopup_Error := True;
{$IFDEF FPC}
{$ELSE}
  FCmd := TDelphiCommand.Create(self);
{$ENDIF}
end;

destructor TWebDriver.Destroy;
begin
  if FProcessInfo.hProcess <> 0 then
    TerminateWebDriver;
  inherited;
end;

procedure TWebDriver.Assign(Source: TPersistent);
var
  WD: TWebDriver;
begin
  inherited;
  if Source is TWebDriver then
  begin
    WD := Source as TWebDriver;
    self.Address := WD.Address;
    self.Port := WD.Port;
    self.Path := WD.Path;
    self.Timeout := WD.Timeout;
  end;
end;

function TWebDriver.BuildParams: string;
begin

end;

procedure TWebDriver.CloseWindow(ParamSessionID: string = '');
var
  Command: string;
begin
  if ParamSessionID <> '' then
    Command := Host + '/session/' + ParamSessionID + '/window'
  else
    Command := Host + '/session/' + FSessionID + '/window';
  ExecuteCommand(cDelete, Command);
end;

procedure TWebDriver.CutImage(Pic: string; X, Y, Width, Height: Integer;
  var bmp: TBitmap);
var
  png: TPngImage;
  Encd: TBase64Encoding;
  Stream: TMemoryStream;
  Byts: TBytes;
  REctS, REctD: TRect;
begin
  Encd := TBase64Encoding.Create;
  Stream := TMemoryStream.Create;
  png := TPngImage.Create;
  try
    Byts := Encd.DecodeStringToBytes(Pic);
    Stream.Write(Byts[0], Length(Byts));
    Stream.Position := 0;
    png.LoadFromStream(Stream);
    REctS.Left := X;
    REctS.Top := Y;
    REctS.Width := Width;
    REctS.Height := Height;

    REctD.Left := 0;
    REctD.Top := 0;
    REctD.Width := Width;
    REctD.Height := Height;

    bmp.Width := Width;
    bmp.Height := Height;
    bmp.Canvas.CopyRect(REctD, png.Canvas, REctS);
    // bitblt(pngd.Canvas.Handle,0,0,Width,height,png.Canvas.Handle,X,Y,SRCCOPY);
  finally
    FreeAndNil(png);
    FreeAndNil(Stream);
    FreeAndNil(Encd);
  end;
end;

procedure TWebDriver.DeleteSession(ParamSessionID: string = '');
var
  Command: string;
begin
  if ParamSessionID <> '' then
  begin
    Command := Host + '/session/' + ParamSessionID;
  end
  else
  begin
    Command := Host + '/session/' + FSessionID;
  end;
  ExecuteCommand(cDelete, Command);
end;

function TWebDriver.ExecuteScript(const Script: string;
  const Args: string = '[]'): string;
var
  Command: string;
  Data: string;
  Resp: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    Command := Host + '/session/' + FSessionID + '/execute/sync';
    Json.S['script'] := Script;
    Json.A['args'].FromJSON(Args);
    Data := Json.ToJSON();
    Resp := ExecuteCommand(cPost, Command, Data);
    result := ProcResponse(Resp);
  Finally
    FreeAndNil(Json);
  end;
end;

function TWebDriver.FindElementByID(const ID: string): TWebElement;
begin
  result := FindElement('id', ID);
end;

function TWebDriver.FindElementByTag(const TagName: string): TWebElement;
begin
  result := FindElement('tag name', TagName);
end;

function TWebDriver.FindElementByClassName(const ClasName: string): TWebElement;
begin
  result := FindElement('class name', ClassName);
end;

function TWebDriver.FindElement(UsingName, KeyName: string): TWebElement;
var
  Command: string;
  Data: string;
  Resp: string;
  JsonData: string;
  Json: TJsonObject;
begin
  Command := Host + '/session/' + FSessionID + '/element';
  if FW3C then
  begin
    if SameText(UsingName, 'id') then
    begin
      UsingName := 'css selector';
      KeyName := format('[id="%s" ]', [KeyName]);
    end
    else if SameText(UsingName, 'tag name') then
    begin
      UsingName := 'css selector';
    end
    else if SameText(UsingName, 'class name') then
    begin
      UsingName := 'css selector';
      KeyName := format('.%s', [KeyName]);
    end
    else if SameText(UsingName, 'name') then
    begin
      UsingName := 'css selector';
      KeyName := format('[name="%s"]', [KeyName]);
    end;
  end;

  Json := TJsonObject.Create;
  try
    Json.S['using'] := UsingName;
    Json.S['value'] := KeyName;
    Data := Json.ToJSON();
  finally
    Json.Free;
  end;
  Resp := ExecuteCommand(cPost, Command, Data);
  JsonData := ProcResponse(Resp);

  result.WebDriver := self;
  result.UsingName := UsingName;
  result.KeyName := KeyName;
  if not HasError then
  begin
    result.W3C := FW3C;
    result.ElementData := JsonData;
  end
  else
    result.ElementData := '';
end;

function TWebDriver.FindElementByLinkText(const LinkText: string): TWebElement;
begin
  result := FindElement('link text', LinkText);
end;

function TWebDriver.FindElementByXPath(XPath: string): TWebElement;
begin
  result := FindElement('xpath', XPath);
end;

function TWebDriver.GetAllSession: string;
var
  Command: string;
  Resp: string;
begin
  Command := Host + '/sessions';
  Resp := ExecuteCommand(cGet, Command);
  if Resp <> '' then
  begin
    result := ProcResponse(Resp);
  end
  else
  begin
    result := '';
  end;
end;

function TWebDriver.GetCurrentWindowHandle: string;
var
  Command: string;
  Resp: string;
begin
  Command := Host + '/session/' + FSessionID + '/window_handle';
  Resp := ExecuteCommand(cGet, Command);
  result := ProcResponse(Resp);
end;

function TWebDriver.GetHost: string;
begin
  result := format('http://%s:%d%s', [FAddress, FPort, FPath]);
end;

procedure TWebDriver.GetURL(const URL: string);
var
  Command: string;
  Data: string;
  Resp: string;
  Json: TJsonObject;
begin
  Command := Host + '/session/' + FSessionID + '/url';
  Json := TJsonObject.Create;
  try
    Json.S['url'] := URL;
    Json.S['sessionid'] := FSessionID;
    Data := Json.ToJSON();
    Resp := ExecuteCommand(cPost, Command, Data);
    ProcResponse(Resp);
    if not HasError then
      WaitForLoaded;
  finally
    FreeAndNil(Json);
  end;
end;

procedure TWebDriver.Implicitly_Wait(const waitTime: Double);
var
  Command: string;
  Data: string;
  Resp: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    if W3C then
    begin
      Command := Host + '/session/' + FSessionID + '/timeouts';
      Json.F['implicit'] := waitTime;
    end
    else
    begin
      Command := Host + '/session/' + FSessionID + '/timeouts/implicit_wait';
      Json.F['ms'] := waitTime;
      Json.S['session'] := FSessionID;
      Data := Json.ToJSON();
    end;
    Resp := ExecuteCommand(cPost, Command, Data);
    ProcResponse(Resp);
  finally
    FreeAndNil(Json);
  end;
end;

procedure TWebDriver.Quit;
begin
  if FSessionID <> '' then
  begin
    DeleteSession(FSessionID);
    FSessionID := '';
  end;
end;

procedure TWebDriver.Refresh(ParamSessionID: string = '');
var
  Command: string;
  Data: string;
  Resp: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    if ParamSessionID <> '' then
      Command := Host + '/session/' + ParamSessionID + '/refresh'
    else
      Command := Host + '/session/' + FSessionID + '/refresh';
    Json.S['sessionid'] := FSessionID;
    Data := Json.ToJSON();
    Resp := ExecuteCommand(cPost, Command, Data);
    ProcResponse(Resp);
  finally
    FreeAndNil(Json);
  end;
end;

procedure TWebDriver.StartDriver(const ExeName: string;
  const Args: string = '');
var
  Command: string;
begin
  if not FileExists(ExeName) then
    raise Exception.Create('driver file not exists.' + ExeName);
  FDriverName := ExeName;
  if FProcessInfo.hProcess <> 0 then
    Exit;
  FillChar(FStartupInfo, SizeOf(FStartupInfo), 0);
  FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
  FStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  FStartupInfo.wShowWindow := SW_HIDE;
  if Args = '' then
    Command := self.BuildParams
  else
    Command := Args;

  if CreateProcess(nil, Pchar(ExeName + ' ' + Command), nil, nil, False,
    NORMAL_PRIORITY_CLASS, nil, nil, FStartupInfo, FProcessInfo) then
  begin

  end;
end;

function TWebDriver.FindElementsByXPath(XPath: string): TWebElements;
begin
  result := FindElements('xpath', XPath);
end;

function TWebDriver.FindElementsByTag(const TagName: string): TWebElements;
begin
  result := FindElements('tag name', TagName);
end;

function TWebDriver.FindElementsByLinkText(const LinkText: string)
  : TWebElements;
begin
  result := FindElements('link text', LinkText);
end;

function TWebDriver.FindElementsByID(const ID: string): TWebElements;
begin
  result := FindElements('id', ID);
end;

function TWebDriver.FindElementsByClassName(const ClasName: string)
  : TWebElements;
begin
  result := FindElements('class name', ClassName);
end;

function TWebDriver.FindElements(const UsingName, KeyName: string)
  : TWebElements;
var
  Command: string;
  Data: string;
  Resp: string;
  Json: TJsonObject;
  JsonData: string;
begin
  Json := TJsonObject.Create;
  try
    Command := Host + '/session/' + FSessionID + '/elements';
    Json.S['value'] := KeyName;
    Json.S['sessionid'] := FSessionID;
    Json.S['using'] := UsingName;
    Data := Json.ToJSON(False);
  finally
    FreeAndNil(Json);
  end;
  Resp := ExecuteCommand(cPost, Command, Data);
  JsonData := ProcResponse(Resp);

  result.WebDriver := self;
  result.UsingName := UsingName;
  result.KeyName := KeyName;
  if not HasError then
  begin
    result.W3C := FW3C;
    result.ElementsData := JsonData;
  end
  else
    result.ElementsData := '';
end;

procedure TWebDriver.Set_Window_Size(const Width, Height: Integer;
  WindowHandle: string = 'current');
var
  Command: string;
  Data: string;
  Resp: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    if W3C then
    begin
      Command := Host + '/session/' + FSessionID + '/window/rect';
      Json.I['width'] := Width;
      Json.I['height'] := Height;
      Json.S['windowHandle'] := WindowHandle;
    end
    else
    begin
      Command := Host + '/session/' + FSessionID + '/window/' + WindowHandle
        + '/size';

      Json.I['width'] := Width;
      Json.I['height'] := Height;

    end;

    Data := Json.ToJSON();
    Resp := ExecuteCommand(cPost, Command, Data);
    ProcResponse(Resp);
  finally
    FreeAndNil(Json);
  end;
end;

procedure TWebDriver.TerminateWebDriver;
begin
  TerminateProcess(FProcessInfo.hProcess, 0);
  FillChar(FStartupInfo, SizeOf(FStartupInfo), 0);
  FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
end;

procedure TWebDriver.Clear;
var
  AllSession: string;
  Json: TJsonArray;
  Session: string;
  I: Integer;
begin
  AllSession := GetAllSession;
  Json := TJsonArray.Create;
  try
    if AllSession <> '' then
    begin
      Json.FromJSON(AllSession);
      for I := 0 to Json.Count - 1 do
      begin
        Session := Json.O[I].S['id'];
        CloseWindow(Session);
        DeleteSession(Session);
      end;
    end;
  finally
    FreeAndNil(Json);
  end;
end;

procedure TWebDriver.DeleteAllCookie;
var
  Command: string;
begin
  Command := Host + '/session/' + FSessionID + '/cookie';
  ExecuteCommand(cDelete, Command)
end;

procedure TWebDriver.DeleteCookie(const cookieName: string);
var
  Command: string;
begin
  Command := Host + '/session/' + FSessionID + '/cookie' + '/' + cookieName;
  ExecuteCommand(cDelete, Command);
end;

function TWebDriver.GetAllCookie: string;
var
  S: string;
  I: Integer;
  aryJson: TJsonArray;
  tmpJson: TJsonObject;
begin
  result := '';
  S := GetAllCookieJsonArray;
  aryJson := TJsonBaseObject.Parse(S) as TJsonArray;
  if aryJson <> nil then
  begin
    // 数组JSON
    for I := 0 to aryJson.Count - 1 do
    begin
      tmpJson := aryJson.O[I];
      if result = '' then
        result := tmpJson.S['name'] + '=' + tmpJson.S['value']
      else
        result := result + '; ' + tmpJson.S['name'] + '=' + tmpJson.S['value'];
    end;
    aryJson.Free;
  end;
end;

function TWebDriver.GetTimeout: Integer;
begin
  result := FCmd.Timeout;
end;

procedure TWebDriver.PageLoadTimeout(const Timeout: Integer);
var
  Command: string;
  Data: string;
  Resp: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    Command := Host + '/session/' + FSessionID + '/timeouts';
    Json.Clear;
    Json.S['name'] := 'pageLoad';
    Json.I['value'] := Timeout;
    Data := Json.ToJSON(False);
  finally
    FreeAndNil(Json);
  end;
  Resp := ExecuteCommand(cPost, Command, Data);
  ProcResponse(Resp);
end;

function TWebDriver.AddCookie(cookieName, cookieValue: string): string;
var
  Command: string;
  Data: string;
  Resp: string;
  Json, ckJson: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    ckJson := Json.O['cookie'];
    ckJson.S['name'] := cookieName;
    ckJson.S['value'] := cookieValue;
    Command := Host + '/session/' + FSessionID + '/cookie';
    Data := Json.ToJSON(True);
  finally
    FreeAndNil(Json);
  end;
  Resp := ExecuteCommand(cPost, Command, Data);
  ProcResponse(Resp);
end;

procedure TWebDriver.BmpToPng(bmp: TBitmap; const FileName: string);
var
  png: TPngImage;
begin
  png := TPngImage.Create;
  try
    png.Assign(bmp);
    png.SaveToFile(FileName);
  finally
    FreeAndNil(png);
  end;
end;

procedure TWebDriver.Base64ToBmp(Pic: string; var bmp: TBitmap);
var
  png: TPngImage;
  Encd: TBase64Encoding;
  Stream: TMemoryStream;
  Byts: TBytes;
  REctS, REctD: TRect;
begin
  Encd := TBase64Encoding.Create;
  Stream := TMemoryStream.Create;
  png := TPngImage.Create;
  try
    Byts := Encd.DecodeStringToBytes(Pic);
    Stream.Write(Byts[0], Length(Byts));
    Stream.Position := 0;
    png.LoadFromStream(Stream);

    REctS.Left := 0;
    REctS.Top := 0;
    REctS.Width := png.Width;
    REctS.Height := png.Height;

    REctD.Left := 0;
    REctD.Top := 0;
    REctD.Width := png.Width;
    REctD.Height := png.Height;

    bmp.Width := png.Width;
    bmp.Height := png.Height;
    bmp.Canvas.CopyRect(REctD, png.Canvas, REctS);
    // bitblt(pngd.Canvas.Handle,0,0,Width,height,png.Canvas.Handle,X,Y,SRCCOPY);
  finally
    FreeAndNil(png);
    FreeAndNil(Stream);
    FreeAndNil(Encd);
  end;
end;

function TWebDriver.ExecuteCommand(const CommandType: TCommandType;
  const Command: string; const Param: string = ''): string;
begin
  FErrorMessage := '';
  case CommandType of
    cGet:
      begin
        result := FCmd.ExecuteGet(Command);
      end;
    cPost:
      begin
        result := FCmd.ExecutePost(Command, Param);
      end;
    cDelete:
      begin
        FCmd.ExecuteDelete(Command);
      end;

  end;
  if Assigned(FOnResponse) then
  begin
    FOnResponse(result);
  end;

end;

procedure TWebDriver.ExecuteScriptByASync(const Script: string;
  const Args: string = '[]');
var
  Command: string;
  Data: string;
  Resp: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    Command := Host + '/session/' + FSessionID + '/execute/async';
    Json.S['script'] := Script;
    Json.A['args'].FromJSON(Args);
    Json.S['sessionId'] := FSessionID;
    Data := Json.ToJSON();
    Resp := ExecuteCommand(cPost, Command, Data);
    ProcResponse(Resp);
  Finally
    FreeAndNil(Json);
  end;
end;

function TWebDriver.GetDocument: string;
var
  Command: string;
begin
  result := '';
  if FSessionID <> '' then
  begin
    Command := Host + '/session/' + FSessionID + '/source';
    result := ProcResponse(ExecuteCommand(cGet, Command));
  end;
end;

function TWebDriver.GetAllCookieJsonArray: string;
var
  Command: string;
  Resp: string;
begin
  Command := Host + '/session/' + FSessionID + '/cookie';
  Resp := ExecuteCommand(cGet, Command);
  if Resp <> '' then
    result := ProcResponse(Resp)
  else
    result := '[]';
end;

function TWebDriver.GetCookieByName(cookieName: string): string;
var
  Command: string;
  Resp: string;
  S: string;
  I: Integer;
  aryJson: TJsonArray;
  tmpJson: TJsonObject;
begin
  // 用标准接口返回是错误,故用此方法
  Command := Host + '/session/' + FSessionID + '/cookie';
  Resp := ExecuteCommand(cGet, Command);
  if Resp <> '' then
    S := ProcResponse(Resp)
  else
    S := '[]';

  aryJson := TJsonBaseObject.Parse(S) as TJsonArray;
  if aryJson <> nil then
  begin
    // 数组JSON
    for I := 0 to aryJson.Count - 1 do
    begin
      tmpJson := aryJson.O[I];
      if tmpJson.S['name'] = cookieName then
      begin
        result := tmpJson.S['value'];
        break;
      end;
    end;
    aryJson.Free;
  end;
end;

function TWebDriver.GetCurUrl: string;
var
  Command: string;
  Resp: string;
begin
  Command := Host + '/session/' + FSessionID + '/url';
  Resp := ExecuteCommand(cGet, Command);
  result := ProcResponse(Resp);
end;

function TWebDriver.GetDriverIsRunning: Boolean;
begin
  result := FProcessInfo.hProcess <> 0;
end;

function TWebDriver.GetHasError: Boolean;
begin
  result := FErrorMessage <> '';
end;

function TWebDriver.ProcResponse(const Resp: string): string;
var
  Json, Obj: TJsonObject;
  jType: TJsonDataType;
begin
  FErrorMessage := '';
  Json := TJsonObject.Create;
  try
    if Resp <> '' then
    begin
      Json.FromJSON(Resp);
      if Json.Contains('value') then
      begin
        // success
        jType := Json.Types['value'];
        case jType of
          jdtString, jdtInt, jdtLong, jdtULong, jdtFloat, jdtDateTime, jdtBool:
            begin
              result := Json.S['value'];
            end;
          jdtObject:
            begin
              Obj := Json.O['value'];
              // obj 有时是 null
              if Assigned(Obj) then
              begin
                // 可能有错误信息在这里返回
                if Obj.Contains('message') then
                  FErrorMessage := Obj.S['message']
                else
                  result := Obj.ToJSON();
              end
              else
                result := '';
            end;
          jdtArray:
            begin
              result := Json.A['value'].ToJSON();
            end;
        else
          result := Json.S['value'];
        end;
      end
      else
      begin
        // falid
        if Json.Contains('message') then
          FErrorMessage := Json.S['message']
        else
          FErrorMessage := Resp;
        result := '';
        if FPopup_Error then
          raise Exception.Create(FErrorMessage);
      end;
    end
    else
    begin
      FErrorMessage := 'http request error';
      result := '';
    end;
  finally
    FreeAndNil(Json);
  end;
end;

procedure TWebDriver.SaveCurDocToFile(const FileName: string);
var
  lst:TStringList;
begin
  lst :=Tstringlist.Create;
  try
    lst.Text :=GetDocument;
    lst.SaveToFile(FileName);
  finally
    FreeandNil(lst);
  end;
end;

procedure TWebDriver.ScreenShot(var bmp: TBitmap);
var
  Command: string;
  Resp: string;
  Pic: string;
  FJson: TJsonObject;
begin
  Command := Host + '/session/' + SessionID + '/screenshot';
  Resp := ExecuteCommand(cGet, Command);
  FJson := TJsonObject.Create;
  try
    FJson.FromJSON(Resp);
    if not HasError then
    begin
      Pic := FJson.S['value'];
      Base64ToBmp(Pic, bmp);
    end;
  finally
    FJson.Free;
  end;
end;

procedure TWebDriver.ScreenShot(const FileName: string);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    ScreenShot(bmp);
    BmpToPng(bmp, FileName);
  finally
    FreeAndNil(bmp);
  end;
end;

procedure TWebDriver.SetTimeout(const Value: Integer);
begin
  FCmd.Timeout := Value;
end;

procedure TWebDriver.SwitchToFrame(const FrameID: string);
var
  Command: string;
  Data: string;
  Resp: string;
  Element: TWebElement;
begin
  Command := Host + '/session/' + FSessionID + '/frame';
  Element := FindElementByID(FrameID);
  if Element.ElementData <> '' then
  begin
    Data := format('{"id": {"ELEMENT": "%s"}}', [Element.Value]);
    Resp := ExecuteCommand(cPost, Command, Data);
    ProcResponse(Resp);
  end;
end;

procedure TWebDriver.SwitchToParentFrame;
var
  Command: string;
  Data: string;
  Resp: string;
begin
  Command := Host + '/session/' + FSessionID + '/frame/parent';
  Data := '{"sessionId":"' + FSessionID + '"}';
  Resp := ExecuteCommand(cPost, Command, Data);
  ProcResponse(Resp);
end;

procedure TWebDriver.WaitForLoaded;
var
  I: Integer;
  isMainThread: Boolean;
  j: Integer;
begin
  isMainThread := (GetCurrentThreadId = MainThreadId);
  I := 0;
  while I < Timeout do
  begin
    for j := 1 to 100 do
    begin
      sleep(10);
      if isMainThread then
        // Application.ProcessMessages;
        CheckSynchronize(0);
    end;
    Inc(I, 1000);
    if ExecuteScript('return document.readyState') = 'complete' then
      break;
  end;
end;

constructor TPhantomjs.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 8080;
  FPath := '/wd/hub';
  FCookieFiles := '';
  FDiskCache := False;
  FDiskCachePath := '';
end;

procedure TPhantomjs.Assign(Source: TPersistent);
var
  WD: TPhantomjs;
begin
  inherited;
  if Source is TPhantomjs then
  begin
    WD := Source as TPhantomjs;
    self.CookieFiles := WD.CookieFiles;
    self.DiskCache := WD.DiskCache;
    self.DiskCachePath := WD.DiskCachePath;
  end;
end;

function TPhantomjs.BuildParams: string;
begin

  result := result + '  --webdriver=' + Address + ':' + inttostr(Port);
  if CookieFiles <> '' then
  begin
    result := result + ' --cookies-file=' + CookieFiles;
  end;
  if FDiskCache then
  begin
    result := result + ' --disk-cache=true';
  end
  else
  begin
    result := result + ' --disk-cache=false';
  end;
  if DiskCachePath <> '' then
  begin
    result := result + ' --disk-cache-path=' + DiskCachePath;
  end;

  if FLogFile <> '' then
  begin
    result := result + ' --webdriver-logfile=' + FLogFile;
  end;
end;

function TPhantomjs.ExecuteScript(const Script: string;
  const Args: string = '[]'): string;
var
  Command: string;
  Data: string;
  Resp: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    Command := Host + '/session/' + FSessionID + '/execute';
    Json.S['script'] := Script;
    Json.A['args'].FromJSON(Args);
    Data := Json.ToJSON();
    // Resp := FCmd.ExecutePost(command, Data);
    Resp := ExecuteCommand(cPost, Command, Data);
    result := ProcResponse(Resp);
  finally
    FreeAndNil(Json);
  end;
end;

function TPhantomjs.Execute_Phantom_Script(const Script: string; const Args:
    string = '[]'): string;
var
  Command: string;
  Data: string;
  Resp: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    Command := Host + '/session/' + FSessionID + '/phantom/execute';
    Json.S['script'] := Script;
    Json.A['args'].FromJSON(Args);
    Data := Json.ToJSON();
    Resp := ExecuteCommand(cPost, Command, Data);
    result := ProcResponse(Resp);
  Finally
    FreeAndNil(Json);
  end;
end;

function TPhantomjs.NewSession: string;
const
  Phantomjs_PARAM =
    '{"desiredCapabilities": {"takesScreenshot":false,"browserName":"phantomjs",'
    + '"phantomjs.page.settings.userAgent": "Mozilla/5.0 (Windows NT 6.2; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1667.0 Safari/537.36"'
    + ' , "platform": "windows", "version": "", "javascriptEnabled": true},' +
    '"capabilities": {"takesScreenshot":false,"browserName": "phantomjs", "firstMatch": [],'
    + '"phantomjs.page.settings.userAgent": "Mozilla/5.0 (Windows NT 6.2; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1667.0 Safari/537.36"'
    + ',"platform": "windows", "alwaysMatch": {}, "javascriptEnabled": true, "version": ""}}';
var
  Command: string;
  Resp: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    result := '';
    Command := Host + '/session';
    Resp := ExecuteCommand(cPost, Command, Phantomjs_PARAM);
    if Resp <> '' then
    begin
      Json.FromJSON(Resp);
      FW3C := not Json.Contains('status');
      if not Json.Contains('sessionId') then
        Json.FromJSON(Json.O['value'].ToJSON());
      FSessionID := Json.S['sessionId'];
      if FSessionID <> '' then
      begin
        result := FSessionID;
        FErrorMessage := '';
      end
      else
      begin
        if Json.Contains('message') then
          FErrorMessage := Json.S['message']
        else
          FErrorMessage := Resp;
      end;
    end
    else
    begin
      FErrorMessage := 'time out';
    end;
  finally
    FreeAndNil(Json);
  end;
end;

constructor TFireFoxDriver.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 4444;
  FnewVersion :=false;
  FPath := '/wd/hub'; // 旧版,我目前用的旧版
  // FPath:=''; 新版
  FBrowserFileName := '';

end;

function TFireFoxDriver.BuildParams: string;
begin
  result := ' --port ' + inttostr(FPort);
end;

function TFireFoxDriver.NewSession: string;
const
  Firefox_Param =
    '{"capabilities": {"firstMatch": [{}], "alwaysMatch": {"browserName": "firefox", '
    + '"acceptInsecureCerts": true, "moz:firefoxOptions": ' +
    '{"binary": "%s"}}}, "desiredCapabilities": {"browserName": "firefox", "acceptInsecureCerts": true, "moz:firefoxOptions": {"binary": "%s"}}}';
var
  Command: string;
  Resp: string;
  JsStr: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    if FBrowserFileName <> '' then
      if not FileExists(FBrowserFileName) then
      begin
        raise Exception.Create('firefox file is not exits.' + FBrowserFileName +
          '.please setup browserfile property');
      end;

    result := '';
    if FnewVersion then
    begin
      FPath :='';

    end else
    begin
      FPath := '/wd/hub';

    end;
    Command := Host + '/session';
    JsStr := ReplaceStr(FBrowserFileName, '\', '\\');
    Resp := ExecuteCommand(cPost, Command, format(Firefox_Param,
      [JsStr, JsStr]));
    if Resp <> '' then
    begin
      Json.FromJSON(Resp);
      FW3C := not Json.Contains('status');
      if not Json.Contains('sessionId') then
        Json.FromJSON(Json.O['value'].ToJSON());
      FSessionID := Json.S['sessionId'];
      if FSessionID <> '' then
      begin
        result := FSessionID;
        FErrorMessage := '';
      end
      else
      begin
        if Json.Contains('message') then
          FErrorMessage := Json.S['message']
        else
          FErrorMessage := Resp;
      end;
    end
    else
    begin
      FErrorMessage := 'time out';
    end;
  finally
    FreeAndNil(Json);
  end;
end;

procedure TFireFoxDriver.StartDriver(const ExeName: string;
  const Args: string = '');
var
  Command: string;
begin
  if not FileExists(ExeName) then
    raise Exception.Create('driver file not exists.' + ExeName);
  FDriverName := ExeName;
  if FProcessInfo.hProcess <> 0 then
    Exit;
  FillChar(FStartupInfo, SizeOf(FStartupInfo), 0);
  FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
  FStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  FStartupInfo.wShowWindow := SW_HIDE;
  if Args = '' then
    Command := self.BuildParams
  else
    Command := Args;

  if CreateProcess(nil, Pchar(ExeName + ' ' + Command), nil, nil, False,
    NORMAL_PRIORITY_CLASS, nil, nil, FStartupInfo, FProcessInfo) then
  begin

  end;
end;

constructor TIEDriver.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 5555;
  FPath := '';
end;

function TIEDriver.BuildParams: string;
begin
  result := ' /port=' + inttostr(FPort);
end;

function TIEDriver.NewSession: string;
const
  IE_Param =
    '{"capabilities": {"firstMatch": [{}], "alwaysMatch": {"browserName": "internet explorer", "platformName": "windows"}}, "desiredCapabilities": {"browserName": "internet explorer", "platform": "WINDOWS"}}';
var
  Command: string;
  Resp: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    result := '';
    Command := Host + '/session';
    Resp := ExecuteCommand(cPost, Command, IE_Param);
    if Resp <> '' then
    begin
      Json.FromJSON(Resp);
      FW3C := not Json.Contains('status');
      if not Json.Contains('sessionId') then
        Json.FromJSON(Json.O['value'].ToJSON());
      FSessionID := Json.S['sessionId'];
      if FSessionID <> '' then
      begin
        result := FSessionID;
        FErrorMessage := '';
      end
      else
      begin
        if Json.Contains('message') then
          FErrorMessage := Json.S['message']
        else
          FErrorMessage := Resp;
        if FPopup_Error then
          raise Exception.Create(FErrorMessage);
      end;
    end
    else
    begin
      FErrorMessage := 'time out';
    end;
  finally
    FreeAndNil(Json);
  end;
end;

constructor TChromeDriver.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 9515;
  FPath := '';
end;

function TChromeDriver.BuildParams: string;
begin
  result := ' --port=' + inttostr(FPort);
end;

function TChromeDriver.NewSession: string;
const
  Chrome_Param =
    '{"capabilities": {"firstMatch": [{}], "alwaysMatch": {"browserName": "chrome",'
    + ' "platformName": "any", "goog:chromeOptions": {"extensions": [], "args": []}}},'
    + ' "desiredCapabilities": {"browserName": "chrome", "version": "", "platform": "ANY",'
    + ' "goog:chromeOptions": {"extensions": [], "args": []}}}';
var
  Command: string;
  Resp: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    result := '';
    Command := Host + '/session';
    Resp := ExecuteCommand(cPost, Command, Chrome_Param);
    if Resp <> '' then
    begin
      Json.FromJSON(Resp);
      FW3C := not Json.Contains('status');
      if not Json.Contains('sessionId') then
        Json.FromJSON(Json.O['value'].ToJSON());
      FSessionID := Json.S['sessionId'];
      if FSessionID <> '' then
      begin
        result := FSessionID;
        FErrorMessage := '';
      end
      else
      begin
        if Json.Contains('message') then
          FErrorMessage := Json.S['message']
        else
          FErrorMessage := Resp;
      end;
    end
    else
    begin
      FErrorMessage := 'time out';
    end;
  finally
    FreeAndNil(Json);
  end;
end;

procedure TChromeDriver.StartDriver(const ExeName: string;
  const Args: string = '');
var
  Command: string;
begin
  if not FileExists(ExeName) then
    raise Exception.Create('driver file not exists.' + ExeName);
  FDriverName := ExeName;
  if FProcessInfo.hProcess <> 0 then
    Exit;
  FillChar(FStartupInfo, SizeOf(FStartupInfo), 0);
  FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
  FStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  FStartupInfo.wShowWindow := SW_HIDE;
  if Args = '' then
    Command := self.BuildParams
  else
    Command := Args;

  if CreateProcess(nil, Pchar(ExeName + Command), nil, nil, False,
    NORMAL_PRIORITY_CLASS, nil, nil, FStartupInfo, FProcessInfo) then
  begin

  end;
end;

constructor TEdgeDriver.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 7777;
  FPath := '';
  FAddress := 'localhost';
end;

function TEdgeDriver.BuildParams: string;
begin
  result := ' --port=' + inttostr(FPort);
end;

function TEdgeDriver.NewSession: string;
const
  Edge_Param =
    '{"capabilities": {"firstMatch": [{}], "alwaysMatch": {"browserName": "MicrosoftEdge",'
    + ' "platformName": "windows"}}, "desiredCapabilities": {"browserName": "MicrosoftEdge",'
    + ' "version": "", "platform": "WINDOWS"}}';
var
  Command: string;
  Resp: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    result := '';
    Command := Host + '/session';
    Resp := ExecuteCommand(cPost, Command, Edge_Param);
    if Resp <> '' then
    begin
      Json.FromJSON(Resp);
      FW3C := not Json.Contains('status');
      if (not Json.Contains('sessionId')) or
        (Json.Values['sessionId'].VariantValue = null) then
        Json.FromJSON(Json.O['value'].ToJSON());
      FSessionID := Json.S['sessionId'];
      if FSessionID <> '' then
      begin
        result := FSessionID;
        FErrorMessage := '';
      end
      else
      begin
        if Json.Contains('message') then
          FErrorMessage := Json.S['message']
        else
          FErrorMessage := Resp;
      end;
    end
    else
    begin
      FErrorMessage := 'time out';
    end;
  finally
    FreeAndNil(Json);
  end;
end;

procedure TEdgeDriver.SwitchToFrame(const FrameID: string);
var
  Command: string;
  Data: string;
  Resp: string;
begin
  Command := Host + '/session/' + FSessionID + '/frame';
  Data := '{"id": "' + FrameID + '",' + '"sessionId":"' + FSessionID + '"}';
  Resp := ExecuteCommand(cPost, Command, Data);
  ProcResponse(Resp);
end;

function TWebElement.AttributeValue(aName: string): string;
var
  Command: string;
  Ele: string;
  Resp: string;
begin
  result := '';
  if WebDriver <> nil then
  begin
    Ele := Value;
    Command := WebDriver.Host + '/session/' + WebDriver.SessionID + '/element/'
      + Ele + '/attribute/' + aName;
    Resp := WebDriver.ExecuteCommand(cGet, Command);
    result := WebDriver.ProcResponse(Resp);
  end;
end;

function TWebElement.PropertyValue(aName: string): string;
var
  Command: string;
  Ele: string;
  Resp: string;
begin
  result := '';
  if WebDriver <> nil then
  begin
    Ele := Value;
    Command := WebDriver.Host + '/session/' + WebDriver.SessionID + '/element/'
      + Ele + '/property/' + aName;
    Resp := WebDriver.ExecuteCommand(cGet, Command);
    result := WebDriver.ProcResponse(Resp);
  end;
end;

procedure TWebElement.Clear;
var
  Command: string;
  Ele: string;
  Data: string;
  Resp: string;
  FJson: TJsonObject;
begin
  if WebDriver <> nil then
  begin
    Ele := Value;
    if Ele <> '' then
    begin
      Command := WebDriver.Host + '/session/' + WebDriver.SessionID +
        '/element/' + Ele + '/clear';
      FJson := TJsonObject.Create;
      FJson.S['using'] := UsingName;
      FJson.S['value'] := KeyName;
      FJson.S['id'] := Ele;
      Data := FJson.ToJSON(False);
      Resp := WebDriver.ExecuteCommand(cPost, Command, Data);
      WebDriver.ProcResponse(Resp);
    end;
  end;
end;

procedure TWebElement.Click;
var
  Command: string;
  Ele: string;
  Data: string;
  Resp: string;
  FJson: TJsonObject;
begin
  if WebDriver <> nil then
  begin
    Ele := Value;
    if Ele <> '' then
    begin
      Command := WebDriver.Host + '/session/' + WebDriver.SessionID +
        '/element/' + Ele + '/click';
      FJson := TJsonObject.Create;
      FJson.S['using'] := UsingName;
      FJson.S['value'] := KeyName;
      FJson.S['id'] := Ele;
      Data := FJson.ToJSON(False);
      Resp := WebDriver.ExecuteCommand(cPost, Command, Data);
      WebDriver.ProcResponse(Resp);
    end;
  end;
end;

function TWebElement.GetTagName: string;
var
  Command: string;
  Ele: string;
  Resp: string;
begin
  result := '';
  if WebDriver <> nil then
  begin
    Ele := Value;
    if Ele <> '' then
    begin
      Command := WebDriver.Host + '/session/' + WebDriver.SessionID +
        '/element/' + Ele + '/name';
      Resp := WebDriver.ExecuteCommand(cGet, Command);
      result := WebDriver.ProcResponse(Resp);
    end;
  end;
end;

function TWebElement.GetValue: string;
var
  Json: TJsonObject;
begin
  result := '';
  if ElementData <> '' then
  begin
    Json := TJsonObject.Create;
    try
      Json.FromJSON(ElementData);
      if not W3C then
      begin
        result := Json.S['ELEMENT'];
      end
      else
      begin
        result := Json.Items[0].Value;
      end;
    finally
      FreeAndNil(Json);
    end;
  end;
end;

function TWebElement.Location: string;
var
  Command: string;
  Resp: string;
  Ele: string;
begin
  result := '';
  if WebDriver <> nil then
  begin
    Ele := Value;
    if Ele <> '' then
    begin
      if W3C then
        Command := WebDriver.Host + '/session/' + WebDriver.SessionID +
          '/element/' + Ele + '/rect'
      else
        Command := WebDriver.Host + '/session/' + WebDriver.SessionID +
          '/element/' + Ele + '/location';
      Resp := WebDriver.ExecuteCommand(cGet, Command);
      result := WebDriver.ProcResponse(Resp);
    end;
  end;
end;

procedure TWebElement.ScreenShot(const FileName: string);
var
  bmp: TBitmap;
begin
  if WebDriver <> nil then
  begin
    bmp := TBitmap.Create;
    try
      ScreenShot(bmp);
      WebDriver.BmpToPng(bmp, FileName);
    finally
      FreeAndNil(bmp);
    end;
  end;
end;

procedure TWebElement.ScreenShot(var bmp: TBitmap);
var
  Command: string;
  Resp: string;
  Pic: string;
  X, Y, Width, Height: Integer;
  FJson: TJsonObject;
begin
  if WebDriver <> nil then
  begin
    Command := WebDriver.Host + '/session/' + WebDriver.SessionID +
      '/screenshot';
    Resp := WebDriver.ExecuteCommand(cGet, Command);
    FJson := TJsonObject.Create;
    try
      FJson.FromJSON(Resp);
      if not WebDriver.HasError then
      begin
        Pic := FJson.S['value'];
        FJson.FromJSON(Location);

        X := FJson.I['x'];
        Y := FJson.I['y'];
        FJson.FromJSON(Size);
        Width := FJson.I['width'];
        Height := FJson.I['height'];
        WebDriver.CutImage(Pic, X, Y, Width, Height, bmp);
      end;
    finally
      FJson.Free;
    end;
  end;
end;

procedure TWebElement.SendKey(Key: string);
var
  Command: string;
  Data: string;
  Resp: string;
  KeyArr: string;
  I: Integer;
  FJson: TJsonObject;
begin
  Command := WebDriver.Host + '/session/' + WebDriver.SessionID + '/element/' +
    Value + '/value';
  FJson := TJsonObject.Create;
  try
    FJson.Clear;
    KeyArr := '[';
    for I := 1 to Length(Key) do
    begin
      if KeyArr = '[' then
        KeyArr := KeyArr + '"' + Key[I] + '"'
      else
        KeyArr := KeyArr + ',' + '"' + Key[I] + '"';
    end;

    KeyArr := KeyArr + ']';

    FJson.A['value'].FromJSON(KeyArr);
    FJson.S['text'] := Key;
    FJson.S['id'] := Value;
    Data := FJson.ToJSON();
  finally
    FJson.Free;
  end;
  Resp := WebDriver.ExecuteCommand(cPost, Command, Data);
  WebDriver.ProcResponse(Resp);
end;

function TWebElement.GetSize: string;
var
  Command: string;
  Resp: string;
  Ele: string;
begin
  result := '';
  if WebDriver <> nil then
  begin
    Ele := Value;
    if Ele <> '' then
    begin
      if W3C then
        Command := WebDriver.Host + '/session/' + WebDriver.SessionID +
          '/element/' + Ele + '/rect'
      else
        Command := WebDriver.Host + '/session/' + WebDriver.SessionID +
          '/element/' + Ele + '/size';
      Resp := WebDriver.ExecuteCommand(cGet, Command);
      result := WebDriver.ProcResponse(Resp);
    end;
  end;
end;

function TWebElement.GetText: string;
var
  Command: string;
  Ele: string;
  Resp: string;
begin
  result := '';
  if WebDriver <> nil then
  begin
    Ele := Value;
    if Ele <> '' then
    begin
      Command := WebDriver.Host + '/session/' + WebDriver.SessionID +
        '/element/' + Ele + '/text';
      Resp := WebDriver.ExecuteCommand(cGet, Command);
      result := WebDriver.ProcResponse(Resp);
    end;
  end;
end;

function TWebElement.IsEmpty: Boolean;
begin
  Result :=ElementData='' ;
end;

procedure TWebElement.ScreenShot2(var bmp: TBitmap);
var
  Command: string;
  Resp: string;
  Pic: string;
  FJson: TJsonObject;
begin
  if WebDriver <> nil then
  begin
    Command := WebDriver.Host + '/session/' + WebDriver.SessionID + '/element/'
      + Value + '/screenshot';
    Resp := WebDriver.ExecuteCommand(cGet, Command);
    FJson := TJsonObject.Create;
    try
      FJson.FromJSON(Resp);
      if not WebDriver.HasError then
      begin
        Pic := FJson.S['value'];
        WebDriver.Base64ToBmp(Pic, bmp);
      end;
    finally
      FJson.Free;
    end;
  end;
end;

function TWebElements.GetCount: Integer;
var
  Json: TJsonArray;
begin
  result := 0;
  if ElementsData <> '' then
  begin
    Json := TJsonArray.Create;
    try
      Json.FromJSON(ElementsData);
      result := Json.Count;
    finally
      Json.Free;
    end;
  end;
end;

function TWebElements.GetItems(Index: Integer): TWebElement;
var
  Json: TJsonArray;
begin
  if ElementsData <> '' then
  begin
    Json := TJsonArray.Create;
    try
      Json.FromJSON(ElementsData);
      if (index >= 0) and (Index < Json.Count) then
      begin
        result.ElementData := Json.O[Index].ToJSON;
        result.W3C := W3C;
        result.WebDriver := WebDriver;
        result.UsingName := UsingName;
        result.KeyName := KeyName;
      end;
    finally
      Json.Free;
    end;
  end;
end;

end.
