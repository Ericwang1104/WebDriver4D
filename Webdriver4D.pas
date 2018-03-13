unit Webdriver4D;

interface

uses
  Classes, SysUtils, Windows, Vcl.Graphics, WD_http,
{$IFDEF FPC} {$ELSE} WD_httpDelphi, {$ENDIF}
  JsonDataObjects, Winapi.ShlObj;

type
  TDriverType = (btPhantomjs, btIE, btFirefox, btChrome);

  TWebDriver = class(TComponent)
  strict private
    Fcapabilities: string;
    FPort: integer;
    FSessionID: string;
    function BuildParams: string;
    procedure CutImage(const FileName: string; Pic: string;
      X, Y, Width, Height: integer);
    function ProcResponse(const Resp: string): string;
  private
    FAddress: string;
    FBrowserType: TDriverType;
    FCmd: TDriverCommand;
    FCookieFiles: string;
    FDiskCache: Boolean;
    FDiskCachePath: string;
    FErrorMessage: string;
    FHasError: Boolean;
    FLogFile: string;
    FPath: string;
    FJson: TJsonObject;
    FProcessInfo: TProcessInformation;
    FStartupInfo: TStartupInfo;
    function GetHost: string;
    function GetTimeout: integer;
    procedure SaveScreenToFileName(const FileName, Base64File: string);
    procedure SetBrowserType(const Value: TDriverType);
    procedure SetTimeout(const Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartPm(const ExeName: string);
    procedure Assign(Source: TPersistent); override;
    procedure CloseWindow(ParamSessionID: string = '');
    function GetAllCookie: string;
    function GetAllCookieJsonArray: string;
    function GetCookieByName(cookieName: string): string;
    function AddCookie(cookieName, cookieValue: string): string;
    procedure DeleteAllCookie;
    procedure DeleteCookie(const cookieName: string);
    function FindElementByID(const ID: string): string;
    function FindElementByTag(const TagName: string): string;
    function FindElementByClassName(const ClasName: string): string;
    function FindElement(const usingName, KeyName: string): string;
    function FindElements(const usingName, KeyName: string): string;
    function FindElementByLinkText(const LinkText: string): string;
    function FindElementByXPath(XPath: string): string;
    function GetCurrentWindowHandle: string;
    function GetElementAttribute(const Element, attributename: string): string;
    procedure GetURL(const URL: string);
    function GetCurUrl: string;
    function NewSession(BrowserFileName: string = ''): string;
    procedure DeleteSession(ParamSessionID: string = '');
    function GetDocument: string;
    function GetAllSession: string;
    procedure Save_screenshot(const FileName: string);
    procedure Set_Window_Size(const Width, Height: integer;
      WindowHandle: string = 'current');
    procedure ElementClick(const Element: string);
    function Element_Location(const Element: string): string;
    procedure Element_ScreenShort(const Element, FileName: string);
    function Element_Size(const Element: string): string;
    function ExecuteScript(const Script: string;
      const Args: string = '[]'): string;
    function FindElementsByXPath(XPath: string): string;
    function FindElementsByTag(const TagName: string): string;
    function FindElementsByLinkText(const LinkText: string): string;
    function FindElementsByID(const ID: string): string;
    function FindElementsByClassName(const ClasName: string): string;

    procedure Implicitly_Wait(const waitTime: Double);
    procedure PageLoadTimeout(const Timeout: integer);
    procedure Quit;
    procedure Refresh(ParamSessionID: string = '');
    procedure SendKey(const Element, Key: string);
    procedure SwitchToFrame(const FrameID: string);
    procedure TerminatePhantomjs;
    property CookieFiles: string read FCookieFiles write FCookieFiles;
    property DiskCache: Boolean read FDiskCache write FDiskCache;
    property DiskCachePath: string read FDiskCachePath write FDiskCachePath;
    property ErrorMessage: string read FErrorMessage;
    property HasError: Boolean read FHasError;
    property LogFile: string read FLogFile write FLogFile;
    property Host: string read GetHost;
    property Address: string read FAddress write FAddress;
    property BrowserType: TDriverType read FBrowserType write SetBrowserType;
    property Cmd: TDriverCommand read FCmd write FCmd;
    property Port: integer read FPort write FPort;
    property Path: string read FPath write FPath;
    property SessionID: string read FSessionID write FSessionID;
  published
    procedure Clear;
    property Timeout: integer read GetTimeout write SetTimeout;
  end;

implementation

uses
  System.NetEncoding, Vcl.Imaging.pngimage, System.StrUtils, Winapi.TlHelp32;

constructor TWebDriver.Create(AOwner: TComponent);
begin
  inherited;
  FJson := TJsonObject.Create;
  FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
  FAddress := '127.0.0.1';
  BrowserType := btPhantomjs;

  FCookieFiles := '';
  FDiskCache := false;
  FDiskCachePath := '';
  FLogFile := '';
  FHasError := false;
  FErrorMessage := '';
{$IFDEF FPC}
{$ELSE}
  FCmd := TDelphiCommand.Create(self);
{$ENDIF}
end;

destructor TWebDriver.Destroy;
begin
  FreeAndNil(FJson);
  FreeAndNil(FCmd);
  if FProcessInfo.hProcess <> 0 then
    TerminatePhantomjs;
  inherited;
end;

procedure TWebDriver.Assign(Source: TPersistent);
var
  Ph: TWebDriver;
begin
  inherited;
  if Source is TWebDriver then
  begin
    Ph := Source as TWebDriver;
    self.Address := Ph.Address;
    self.BrowserType := Ph.BrowserType;
    self.Port := Ph.Port;
    self.Path := Ph.Path;
    self.CookieFiles := Ph.CookieFiles;
    self.DiskCache := Ph.DiskCache;
    self.DiskCachePath := Ph.DiskCachePath;
    self.Timeout := Ph.Timeout;
  end;
end;

function TWebDriver.BuildParams: string;
begin

  result := result + '  --webdriver=' + FAddress + ':' + inttostr(FPort);
  if CookieFiles <> '' then
  begin
    result := result + ' --cookies-file=' + FCookieFiles;
  end;
  if FDiskCache then
  begin
    result := result + ' --disk-cache=true';
  end
  else
  begin
    result := result + ' --disk-cache=false';
  end;
  if FDiskCachePath <> '' then
  begin
    result := result + ' --disk-cache-path=' + FDiskCachePath;
  end;

  if FLogFile <> '' then
  begin
    result := result + ' --webdriver-logfile=' + FLogFile;
  end;
end;

procedure TWebDriver.CloseWindow(ParamSessionID: string = '');
var
  command: string;
begin
  if ParamSessionID <> '' then
    command := Host + '/session/' + ParamSessionID + '/window'
  else
    command := Host + '/session/' + FSessionID + '/window';
  FCmd.ExecuteDelete(command);
end;

procedure TWebDriver.CutImage(const FileName: string; Pic: string;
  X, Y, Width, Height: integer);
var
  png: TPngImage;
  Encd: TBase64Encoding;
  Stream: TMemoryStream;
  Byts: TBytes;
  bmp: TBitmap;
  REctS, REctD: TRect;
begin
  Encd := TBase64Encoding.Create;
  Stream := TMemoryStream.Create;
  bmp := TBitmap.Create;
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
    bmp.Width := Width;
    bmp.Height := Height;

    REctD.Width := Width;
    REctD.Height := Height;

    bmp.Canvas.CopyRect(REctD, png.Canvas, REctS);
    // bitblt(pngd.Canvas.Handle,0,0,Width,height,png.Canvas.Handle,X,Y,SRCCOPY);
    png.Assign(bmp);
    png.SaveToFile(FileName);

  finally
    FreeAndNil(png);
    FreeAndNil(bmp);
    FreeAndNil(Stream);
    FreeAndNil(Encd);
  end;
end;

procedure TWebDriver.DeleteSession(ParamSessionID: string = '');
var
  command: string;
begin
  if ParamSessionID <> '' then
  begin
    command := Host + '/session/' + ParamSessionID;
  end
  else
  begin
    command := Host + '/session/' + FSessionID;
  end;
  FCmd.ExecuteDelete(command);
end;

procedure TWebDriver.ElementClick(const Element: string);
var
  command: string;
  Ele: string;
  Data: string;
  Resp: string;
begin
  FJson.FromJSON(Element);
  Ele := FJson.S['ELEMENT'];
  command := Host + '/session/' + FSessionID + '/element/' + Ele + '/click';
  FJson.S['sessionid'] := FSessionID;
  FJson.S['id'] := Ele;
  Data := FJson.ToJSON(false);
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);

end;

function TWebDriver.Element_Location(const Element: string): string;
var
  command: string;
  Resp: string;
  Ele: string;
begin
  FJson.Clear;
  FJson.FromJSON(Element);
  Ele := FJson.S['ELEMENT'];
  command := Host + '/session/' + FSessionID + '/element/' + Ele + '/location';
  Resp := FCmd.ExecuteGet(command);
  // FJson.FromJSON(Resp);
  result := ProcResponse(Resp);
end;

function TWebDriver.Element_Size(const Element: string): string;
var
  command: string;
  Resp: string;
  Ele: string;
begin
  FJson.Clear;
  FJson.FromJSON(Element);
  Ele := FJson.S['ELEMENT'];
  command := Host + '/session/' + FSessionID + '/element/' + Ele + '/size';
  Resp := FCmd.ExecuteGet(command);
  // FJson.FromJSON(Resp);
  result := ProcResponse(Resp);
end;

function TWebDriver.ExecuteScript(const Script: string;
  const Args: string = '[]'): string;
var
  command: string;
  Data: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/execute';
  FJson.Clear;
  FJson.S['script'] := Script;
  FJson.S['sessionid'] := FSessionID;
  FJson.A['args'].FromJSON(Args);

  Data := FJson.ToJSON();
  Resp := FCmd.ExecutePost(command, Data);
  result := ProcResponse(Resp);
end;

function TWebDriver.FindElementByID(const ID: string): string;
begin
  result := FindElement('id', ID);
end;

function TWebDriver.FindElementByTag(const TagName: string): string;
begin
  result := FindElement('tag name', TagName);
end;

function TWebDriver.FindElementByClassName(const ClasName: string): string;
begin
  result := FindElement('class name', ClassName);
end;

function TWebDriver.FindElement(const usingName, KeyName: string): string;
var
  command: string;
  Data: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/element';
  FJson.Clear;
  FJson.S['value'] := KeyName;
  FJson.S['sessionid'] := FSessionID;
  FJson.S['using'] := usingName;
  Data := FJson.ToJSON();
  Resp := FCmd.ExecutePost(command, Data);
  result := ProcResponse(Resp);
end;

function TWebDriver.FindElementByLinkText(const LinkText: string): string;
begin
  result := FindElement('link text', LinkText);
end;

function TWebDriver.FindElementByXPath(XPath: string): string;
begin
  result := FindElement('xpath', XPath);
end;

function TWebDriver.GetAllSession: string;
var
  command: string;
  Resp: string;
begin
  command := Host + '/sessions';
  Resp := FCmd.ExecuteGet(command);
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
  command: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/window_handle';
  Resp := FCmd.ExecuteGet(command);
  result := ProcResponse(Resp);
end;

function TWebDriver.GetElementAttribute(const Element, attributename
  : string): string;
var
  command: string;
  Ele: string;
  Resp: string;
begin
  FJson.FromJSON(Pchar(Element));
  Ele := FJson.S['ELEMENT'];
  command := Host + '/session/' + FSessionID + '/element/' + Ele + '/attribute/'
    + attributename;
  Resp := FCmd.ExecuteGet(command);
  result := ProcResponse(Resp);
end;

function TWebDriver.GetHost: string;
begin
  result := Format('http://%s:%d%s', [FAddress, FPort, FPath]);
end;

procedure TWebDriver.GetURL(const URL: string);
var
  command: string;
  Data: string;
  Resp: string;
  I: integer;
begin
  command := Host + '/session/' + FSessionID + '/url';
  FJson.Clear;
  FJson.S['url'] := URL;
  FJson.S['sessionid'] := FSessionID;
  Data := FJson.ToJSON();
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);
  if not FHasError then
  begin
    I := 0;
    while I < Timeout do
    begin
      sleep(500);
      Inc(I, 1000);
      if ExecuteScript('return document.readyState') = 'complete' then
        break;
    end;
  end;
end;

procedure TWebDriver.Implicitly_Wait(const waitTime: Double);
var
  command: string;
  Data: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/timeouts/implicit_wait';
  FJson.Clear;
  FJson.F['ms'] := waitTime;
  FJson.S['session'] := FSessionID;
  Data := FJson.ToJSON();
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);
end;

function TWebDriver.NewSession(BrowserFileName: string = ''): string;
const
  Phantomjs_PARAM =
    '{"desiredCapabilities": {"takesScreenshot":false,"browserName":"phantomjs",'
    + '"phantomjs.page.settings.userAgent": "Mozilla/5.0 (Windows NT 6.2; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1667.0 Safari/537.36"'
    + ' , "platform": "windows", "version": "", "javascriptEnabled": true},' +
    '"capabilities": {"takesScreenshot":false,"browserName": "phantomjs", "firstMatch": [],'
    + '"phantomjs.page.settings.userAgent": "Mozilla/5.0 (Windows NT 6.2; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1667.0 Safari/537.36"'
    + ',"platform": "windows", "alwaysMatch": {}, "javascriptEnabled": true, "version": ""}}';
  IE_Param =
    '{"capabilities": {"firstMatch": [], "alwaysMatch": {"browserName": "internet explorer", "version": "", "platform": "WINDOWS"}}, "desiredCapabilities": {"browserName": "internet explorer", "version": "", "platform": "WINDOWS"}}';
  Firefox_Param =
    '{"capabilities": {"firstMatch": [], "alwaysMatch": {"browserName": "firefox", '
    + '"acceptInsecureCerts": true, "moz:firefoxOptions": ' +
    '{"binary": "%s"}}}, "desiredCapabilities": {"browserName": "firefox", "acceptInsecureCerts": true, "moz:firefoxOptions": {"binary": "%s"}}}';
var
  command: string;
  status: string;
  Resp: string;
  JsStr: string;
begin
  command := Host + '/session';
  JsStr := ReplaceStr(BrowserFileName, '\', '\\');
  case BrowserType of
    btPhantomjs:
      Resp := FCmd.ExecutePost(command, Phantomjs_PARAM);
    btIE:
      Resp := FCmd.ExecutePost(command, IE_Param);
    btFirefox:
      Resp := FCmd.ExecutePost(command, Format(Firefox_Param, [JsStr, JsStr]));
  end;
  if Resp <> '' then
  begin
    FJson.FromJSON(Resp);

    if FJson.S['status'] = '0' then
    begin
      FSessionID := FJson.S['sessionId'];
      result := FSessionID;
      status := FJson.S['status'];
      Fcapabilities := FJson.O['value'].ToJSON;
      FHasError := false;
      FErrorMessage := '';
    end
    else
    begin
      FHasError := True;
      FErrorMessage := Resp;
    end;
  end
  else
  begin
    FHasError := True;
    FErrorMessage := 'time out';
  end;
end;

procedure TWebDriver.Quit;
begin
  if FSessionID <> '' then
  begin
    self.DeleteSession(FSessionID);
    FSessionID := '';
  end;
end;

procedure TWebDriver.Refresh(ParamSessionID: string = '');
var
  command: string;
  Data: string;
  Resp: string;
begin
  if ParamSessionID <> '' then
    command := Host + '/session/' + ParamSessionID + '/refresh'
  else
    command := Host + '/session/' + FSessionID + '/refresh';
  FJson.Clear;
  FJson.S['sessionid'] := FSessionID;
  Data := FJson.ToJSON();
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);
  // FJson.FromJSON(Resp);
end;

procedure TWebDriver.SaveScreenToFileName(const FileName, Base64File: string);
var
  Encode: TBase64Encoding;
  bytes: TBytes;
  Fs: TFileStream;
begin
  Encode := TBase64Encoding.Create;
  try
    bytes := Encode.DecodeStringToBytes(Base64File);
    Fs := TFileStream.Create(FileName, fmCreate);
    try
      Fs.Write(bytes, Length(bytes));
    finally
      FreeAndNil(Fs);
    end;
  finally
    FreeAndNil(Encode);
  end;
end;

procedure TWebDriver.StartPm(const ExeName: string);
var
  command: string;
begin
  if FProcessInfo.hProcess <> 0 then
    Exit;
  FillChar(FStartupInfo, SizeOf(FStartupInfo), 0);
  FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
  FStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  FStartupInfo.wShowWindow := SW_HIDE;
  command := self.BuildParams;

  if CreateProcess(Pchar(ExeName), Pchar(command), nil, nil, false,
    NORMAL_PRIORITY_CLASS, nil, nil, FStartupInfo, FProcessInfo) then
  begin

  end;
end;

procedure TWebDriver.Save_screenshot(const FileName: string);
var
  command: string;
  Resp: string;
  Pic: string;
begin
  command := Host + '/session/' + FSessionID + '/screenshot';
  Resp := FCmd.ExecuteGet(command);
  Pic := ProcResponse(Resp);
  if not FHasError then
  begin
    SaveScreenToFileName(FileName, Pic);
  end;
end;

procedure TWebDriver.Element_ScreenShort(const Element, FileName: string);
var
  command: string;
  Resp: string;
  Pic: string;
  Size: string;
  Loc: string;
  X, Y, Width, Height: integer;
  Ele: string;
begin
  FJson.Clear;
  FJson.FromJSON(Element);
  Ele := FJson.S['ELEMENT'];
  { command := Host + '/session/' + FSessionID + '/' + Element +
    '/screenshot'; }
  command := Host + '/session/' + FSessionID + '/screenshot';
  Resp := FCmd.ExecuteGet(command);
  FJson.FromJSON(Resp);
  if not FHasError then
  begin
    Pic := FJson.S['value'];
    Loc := Element_Location(Element);
    FJson.FromJSON(Loc);

    X := FJson.I['x'];
    Y := FJson.I['y'];
    Size := Element_Size(Element);
    FJson.FromJSON(Size);
    Width := FJson.I['width'];
    Height := FJson.I['height'];
    CutImage(FileName, Pic, X, Y, Width, Height);
  end;
end;

function TWebDriver.FindElementsByXPath(XPath: string): string;
begin
  result := FindElements('xpath', XPath);
end;

function TWebDriver.FindElementsByTag(const TagName: string): string;
begin
  result := FindElements('tag name', TagName);
end;

function TWebDriver.FindElementsByLinkText(const LinkText: string): string;
begin
  result := FindElements('link text', LinkText);
end;

function TWebDriver.FindElementsByID(const ID: string): string;
begin
  result := FindElements('id', ID);
end;

function TWebDriver.FindElementsByClassName(const ClasName: string): string;
begin
  result := FindElements('class name', ClassName);
end;

function TWebDriver.FindElements(const usingName, KeyName: string): string;
var
  command: string;
  Data: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/elements';
  FJson.Clear;
  FJson.S['value'] := KeyName;
  FJson.S['sessionid'] := FSessionID;
  FJson.S['using'] := usingName;
  Data := FJson.ToJSON(false);
  Resp := FCmd.ExecutePost(command, Data);
  result := ProcResponse(Resp);
end;

procedure TWebDriver.SendKey(const Element, Key: string);
var
  command: string;
  Ele: string;
  Data: string;
  Resp: string;
  KeyArr: string;
  I: integer;
begin
  FJson.FromJSON(Element);
  Ele := FJson.S['ELEMENT'];
  command := Host + '/session/' + FSessionID + '/element/' + Ele + '/value';

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
  FJson.S['sessionid'] := FSessionID;
  FJson.S['id'] := Ele;
  Data := FJson.ToJSON();
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);
end;

procedure TWebDriver.Set_Window_Size(const Width, Height: integer;
  WindowHandle: string = 'current');
var
  command: string;
  Data: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/window/' + WindowHandle
    + '/size';
  FJson.Clear;
  FJson.I['width'] := Width;
  FJson.I['height'] := Height;
  FJson.S['sessionid'] := FSessionID;
  FJson.S['windowHandle'] := WindowHandle;
  Data := FJson.ToJSON();
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);
end;

procedure TWebDriver.TerminatePhantomjs;
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
  I: integer;
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
  command: string;
begin
  command := Host + '/session/' + FSessionID + '/cookie';
  FCmd.ExecuteDelete(command);
end;

procedure TWebDriver.DeleteCookie(const cookieName: string);
var
  command: string;
begin
  command := Host + '/session/' + FSessionID + '/cookie' + '/' + cookieName;
  FCmd.ExecuteDelete(command);
end;

function TWebDriver.GetAllCookie: string;
var
  S: string;
  I: integer;
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

function TWebDriver.GetTimeout: integer;
begin
  result := FCmd.Timeout;
end;

procedure TWebDriver.PageLoadTimeout(const Timeout: integer);
var
  command: string;
  Data: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/timeouts';
  FJson.Clear;
  FJson.S['type'] := 'page load';
  FJson.S['sessionid'] := FSessionID;
  FJson.I['ms'] := Timeout;
  Data := FJson.ToJSON(false);
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);
end;

function TWebDriver.AddCookie(cookieName, cookieValue: string): string;
var
  command: string;
  Data: string;
  Resp: string;
  ckJson: TJsonObject;
begin
  FJson.Clear;
  ckJson := FJson.O['cookie'];
  ckJson.S['name'] := cookieName;
  ckJson.S['value'] := cookieValue;
  command := Host + '/session/' + FSessionID + '/cookie';
  Data := FJson.ToJSON(True);
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);
end;

function TWebDriver.GetDocument: string;
var
  command: string;
begin
  result := '';
  if FSessionID <> '' then
  begin
    command := Host + '/session/' + FSessionID + '/source';
    result := ProcResponse(FCmd.ExecuteGet(command));
  end;
end;

function TWebDriver.GetAllCookieJsonArray: string;
var
  command: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/cookie';
  Resp := FCmd.ExecuteGet(command);
  if Resp <> '' then
    result := ProcResponse(Resp)
  else
    result := '[]';
end;

function TWebDriver.GetCookieByName(cookieName: string): string;
var
  command: string;
  Resp: string;
  S: string;
  I: integer;
  aryJson: TJsonArray;
  tmpJson: TJsonObject;
begin
  // 用标准接口返回是错误,故用此方法
  command := Host + '/session/' + FSessionID + '/cookie';
  Resp := FCmd.ExecuteGet(command);
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
  command: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/url';
  Resp := FCmd.ExecuteGet(command);
  result := ProcResponse(Resp);
end;

function TWebDriver.ProcResponse(const Resp: string): string;
var
  Json, Obj: TJsonObject;
  jType: TJsonDataType;
begin
  Json := TJsonObject.Create;
  try
    if Resp <> '' then
    begin
      Json.FromJSON(Resp);
      if Json.S['status'] = '0' then
      begin
        // success
        FHasError := false;
        jType := Json.Types['value'];
        case jType of
          jdtString, jdtInt, jdtLong, jdtULong, jdtFloat, jdtDateTime, jdtBool:
            begin
              result := Json.S['value'];
            end;
          jdtObject:
            begin
              Obj := Json.O['value'];
              if Assigned(Obj) then
                result := Obj.ToJSON()
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
        FHasError := True;
        FErrorMessage := Resp;
        result := '';
      end;
    end
    else
    begin
      FHasError := True;
      FErrorMessage := '{status:"1",ErrorMessage:" http request error")';
      result := '';
    end;
  finally
    FreeAndNil(Json);
  end;
end;

procedure TWebDriver.SetBrowserType(const Value: TDriverType);
begin
  FBrowserType := Value;
  case FBrowserType of
    btPhantomjs:
      begin
        FPort := 8080;
        FPath := '/wd/hub';
      end;
    btIE:
      begin
        FPort := 5555;
        FPath := '';
      end;
    btFirefox:
      begin
        FPort := 4444;
        FPath := '/wd/hub';
      end;
  end;

end;

procedure TWebDriver.SetTimeout(const Value: integer);
begin
  FCmd.Timeout := Value;
end;

procedure TWebDriver.SwitchToFrame(const FrameID: string);
var
  command: string;
  Data: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/frame';
  FJson.Clear;
  FJson.S['id'] := FrameID;
  FJson.S['sessionid'] := FSessionID;
  Data := FJson.ToJSON();
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);
end;

end.
