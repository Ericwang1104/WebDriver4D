unit Webdriver4D;

interface

uses
  Classes, SysUtils, Windows, Vcl.Graphics, WD_http,
{$IFDEF FPC} {$ELSE} WD_httpDelphi, {$ENDIF}
  JsonDataObjects, Winapi.ShlObj;

type
  TCommandType = (cGet, cPost, cDelete);
  TDriverType = (btPhantomjs, btIE, btFirefox, btChrome);

  TElement = packed record
    W3C: Boolean;
    ElementData: string;
    function GetElementID: string;
    function GetElementName: string;
  private
  public
  end;

  TExecCommandEvent = procedure(response: string) of object;
  TWebDriverClass = class of TWebDriver;
  TWebDriver = class(TComponent)
  strict private
    procedure CutImage(const FileName: string; Pic: string;
      X, Y, Width, Height: integer);
  private
    FAddress: string;
    FCmd: TDriverCommand;
    FCurrentElement: TElement;
    FErrorMessage: string;
    FOnResponse: TExecCommandEvent;
    FPopup_Error: Boolean;
    FProcessInfo: TProcessInformation;
    FStartupInfo: TStartupInfo;
    function GetDriverIsRunning: Boolean;
    function GetHasError: Boolean;
    function GetHost: string;
    function GetTimeout: integer;
    procedure SaveScreenToFileName(const FileName, Base64File: string);
    procedure SetTimeout(const Value: integer);
  strict protected
    FDriverName: string;
    FLogFile: string;
    FPath: string;
    FPort: integer;
    FSessionID: string;
    FW3C: Boolean;
    function BuildParams: string; virtual;
    function ExecuteCommand(const CommandType: TCommandType;
      const Command: string; const Param: string = ''): string;
    function ProcResponse(const Resp: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    function FindElementByID(const ID: string): string;
    function FindElementByTag(const TagName: string): string;
    function FindElementByClassName(const ClasName: string): string;
    function FindElement(usingName, KeyName: string): string;
    function FindElements(const usingName, KeyName: string): string;
    function FindElementByLinkText(const LinkText: string): string;
    function FindElementByXPath(XPath: string): string;
    function GetCurrentWindowHandle: string;
    function GetElementAttribute(const Element, attributename: string): string;
    procedure GetURL(const URL: string);
    function GetCurUrl: string;
    function NewSession: string; virtual; abstract;
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
    function ExecuteScript(const Script: string; const Args: string = '[]')
      : string; virtual;
    function FindElementByName(const Name: string): string;
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
    procedure SwitchToFrame(const FrameID: string); virtual;
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
    property Port: integer read FPort write FPort;
    property Path: string read FPath write FPath;
    property Popup_Error: Boolean read FPopup_Error write FPopup_Error;
    property SessionID: string read FSessionID write FSessionID;
    property W3C: Boolean read FW3C;
  published
    procedure Clear;
    property Timeout: integer read GetTimeout write SetTimeout;
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
  strict protected
    function BuildParams: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function NewSession: string; override;
    procedure StartDriver(const ExeName: string; const Args: string = ''); override;
    property BrowserFileName: string read FBrowserFileName
      write FBrowserFileName;
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
  System.Variants, webdriver_js_string;

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
  // FCmd.ExecuteDelete(command);
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
  // FCmd.ExecuteDelete(command);
  ExecuteCommand(cDelete, Command);
end;

procedure TWebDriver.ElementClick(const Element: string);
var
  Command: string;
  Ele: string;
  Data: string;
  Resp: string;
  Json:TJsonObject;
begin
  // FJson.FromJSON(Element);
  Json :=TJsonObject.Create;
  try
    Ele := Element;
    Command := Host + '/session/' + FSessionID + '/element/' + Ele + '/click';
    // FJson.S['sessionid'] := FSessionID;
    Json.S['id'] := Ele;
    Data := Json.ToJSON(False);
    // Resp := FCmd.ExecutePost(command, Data);
    Resp := ExecuteCommand(cPost, Command, Data);
    ProcResponse(Resp);
  finally
    FreeAndNil(Json);
  end;
end;

function TWebDriver.Element_Location(const Element: string): string;
var
  Command: string;
  Resp: string;
  Ele: string;
begin
  { FJson.Clear;
    FJson.FromJSON(Element);
    Ele := FJson.S['ELEMENT']; }
  Ele := Element;
  if W3C then
    Command := Host + '/session/' + FSessionID + '/element/' + Ele + '/rect'
  else
    Command := Host + '/session/' + FSessionID + '/element/' + Ele +
      '/location';
  // Resp := FCmd.ExecuteGet(command);
  Resp := ExecuteCommand(cGet, Command);
  result := ProcResponse(Resp);
end;

function TWebDriver.Element_Size(const Element: string): string;
var
  Command: string;
  Resp: string;
  Ele: string;
begin
  //FJson.Clear;
  { FJson.FromJSON(Element);
    Ele := FJson.S['ELEMENT']; }
  Ele := Element;
  if W3C then
    Command := Host + '/session/' + FSessionID + '/element/' + Ele + '/rect'
  else
    Command := Host + '/session/' + FSessionID + '/element/' + Ele + '/size';
  // Resp := FCmd.ExecuteGet(command);
  Resp := ExecuteCommand(cGet, Command);
  result := ProcResponse(Resp);

end;

function TWebDriver.ExecuteScript(const Script: string;
  const Args: string = '[]'): string;
var
  Command: string;
  Data: string;
  Resp: string;
  Json:TJsonObject;
begin
  Json :=TJsonObject.Create;
  try
    Command := Host + '/session/' + FSessionID + '/execute/sync';
    //FJson.Clear;
    //FJson.S['script'] := Script;
    //FJson.A['args'].FromJSON(Args);
    //Data := FJson.ToJSON();
    Json.S['script'] :=Script;
    Json.A['args'].FromJSON(args);
    Data :=Json.ToJSON();
    // Resp := FCmd.ExecutePost(command, Data);
    Resp := ExecuteCommand(cPost, Command, Data);
    result := ProcResponse(Resp);
  Finally
    FreeAndNil(Json);
  end;

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

function TWebDriver.FindElement(usingName, KeyName: string): string;
var
  Command: string;
  Data: string;
  Resp: string;
  JsonData:string;
  Json:TJsonObject;
begin
  Command := Host + '/session/' + FSessionID + '/element';

  if FW3C then
  begin
    if SameText(usingName, 'id') then
    begin
      usingName := 'css selector';
      KeyName := format('[id="%s" ]', [KeyName]);
    end
    else if SameText(usingName, 'tag name') then
    begin
      usingName := 'css selector';
    end
    else if SameText(usingName, 'class name') then
    begin
      usingName := 'css selector';
      KeyName := format('.%s', [KeyName]);
    end
    else if SameText(usingName, 'name') then
    begin
      usingName := 'css selector';
      KeyName := format('[name="%s"]', [KeyName]);

    end;
  end;
  Json :=TJsonObject.Create;
  try
  //FJson.Clear;
  //FJson.S['using'] := usingName;
  //FJson.S['value'] := KeyName;
  // FJson.S['sessionid'] := FSessionID;
  Json.S['using'] :=usingName;
  Json.S['value'] :=KeyName;

  Data := Json.ToJSON();
  // Resp := FCmd.ExecutePost(command, Data);
  Resp := ExecuteCommand(cPost, Command, Data);
  // result := ProcResponse(Resp);
  JsonData :=ProcResponse(Resp);
  if not HasError then
  begin
    FCurrentElement.W3C := FW3C;
    FCurrentElement.ElementData := JsonData;
    result := FCurrentElement.GetElementID;
  end else
  begin
    Result :='';
  end;
  finally
    FreeAndNil(Json);
  end;

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
  Command: string;
  Resp: string;
begin
  Command := Host + '/sessions';
  // Resp := FCmd.ExecuteGet(command);
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
  // Resp := FCmd.ExecuteGet(command);
  Resp := ExecuteCommand(cGet, Command);
  result := ProcResponse(Resp);
end;

function TWebDriver.GetElementAttribute(const Element, attributename
  : string): string;
var
  Command: string;
  Ele: string;
  Resp: string;
  js,args:string;
begin
  // FJson.FromJSON(Pchar(Element));
  // Ele := FJson.S['ELEMENT'];
  if FW3C then
  begin
    //Assert(False,'');
    //ExecuteScript('');
    Ele := Element;
    js :=Format('return (%s).apply(null, arguments);',[ATTRIBUTE_JS]);
    args:='[{"ELEMENT":'+'"'+FCurrentElement.GetElementID+'",'+'"'+FCurrentElement.GetElementName+'"'+
    ':'+'"'+FCurrentElement.GetElementID+'"}'+','+'"'+attributename+'"]';
    result :=Self.ExecuteScript(js,args);
  end else
  begin
    Ele := Element;
    Command := Host + '/session/' + FSessionID + '/element/' + Ele + '/attribute/'
      + attributename;
    // Resp := FCmd.ExecuteGet(command);
    Resp := ExecuteCommand(cGet, Command);
    result := ProcResponse(Resp);
  end;
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
  Json:TJsonObject;
begin
  Command := Host + '/session/' + FSessionID + '/url';
  Json :=TJsonObject.Create;
  try
    Json.S['url'] := URL;
    Json.S['sessionid'] := FSessionID;
    Data := Json.ToJSON();
    // Resp := FCmd.ExecutePost(command, Data);
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
  Json:TJsonObject;
begin
  Json :=TJsonObject.Create;
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
    // Resp := FCmd.ExecutePost(command, Data);
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
  Json:TJsonObject;
begin
  Json :=TJsonObject.Create;
  try
    if ParamSessionID <> '' then
      Command := Host + '/session/' + ParamSessionID + '/refresh'
    else
      Command := Host + '/session/' + FSessionID + '/refresh';
    Json.S['sessionid'] := FSessionID;
    Data := Json.ToJSON();
    // Resp := FCmd.ExecutePost(command, Data);
    Resp := ExecuteCommand(cPost, Command, Data);
    ProcResponse(Resp);
  finally
    FreeAndNil(Json);
  end;
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

procedure TWebDriver.StartDriver(const ExeName: string;
  const Args: string = '');
var
  Command: string;
begin
  if not FileExists(ExeName) then
    raise Exception.Create('driver file not exists.' + ExeName);
  FDriverName :=ExeName;
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

procedure TWebDriver.Save_screenshot(const FileName: string);
var
  Command: string;
  Resp: string;
  Pic: string;
begin
  Command := Host + '/session/' + FSessionID + '/screenshot';
  // Resp := FCmd.ExecuteGet(command);
  Resp := ExecuteCommand(cGet, Command);
  Pic := ProcResponse(Resp);
  if not HasError then
  begin
    SaveScreenToFileName(FileName, Pic);
  end;
end;

procedure TWebDriver.Element_ScreenShort(const Element, FileName: string);
var
  Command: string;
  Resp: string;
  Pic: string;
  Size: string;
  Loc: string;
  X, Y, Width, Height: integer;
  Ele: string;
  Json :TJsonObject;
begin
  { Json.Clear;
    Json.FromJSON(Element); }
  // Ele := Json.S['ELEMENT'];
  Json :=TJsonObject.Create;
  try
    Ele := Element;
    { command := Host + '/session/' + FSessionID + '/' + Element +
      '/screenshot'; }
    Command := Host + '/session/' + FSessionID + '/screenshot';

    // Resp := FCmd.ExecuteGet(command);
    Resp := ExecuteCommand(cGet, Command);
    Json.FromJSON(Resp);
    if not HasError then
    begin
      Pic := Json.S['value'];
      Loc := Element_Location(Element);
      Json.FromJSON(Loc);

      X := Json.I['x'];
      Y := Json.I['y'];
      Size := Element_Size(Element);
      Json.FromJSON(Size);
      Width := Json.I['width'];
      Height := Json.I['height'];
      CutImage(FileName, Pic, X, Y, Width, Height);
    end;
  finally
    FreeAndNil(Json);
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
  Command: string;
  Data: string;
  Resp: string;
  Json :TJsonObject;
begin
  Json :=TJsonObject.Create;
  try
    Command := Host + '/session/' + FSessionID + '/elements';
    Json.S['value'] := KeyName;
    Json.S['sessionid'] := FSessionID;
    Json.S['using'] := usingName;
    Data := Json.ToJSON(False);
    // Resp := FCmd.ExecutePost(command, Data);
    Resp := ExecuteCommand(cPost, Command, Data);
    result := ProcResponse(Resp);
  finally
    FreeAndNil(Json);
  end;
end;

procedure TWebDriver.SendKey(const Element, Key: string);
var
  Command: string;
  Ele: string;
  Data: string;
  Resp: string;
  KeyArr: string;
  I: integer;
  Arr:TJsonArray;
  Json:TJsonObject;
begin
  Json :=TJsonObject.Create;
  try
    Ele := Element;
    Command := Host + '/session/' + FSessionID + '/element/' + Ele + '/value';
    Arr :=Json.A['value'];
    for I := 1 to Length(Key) do
    begin
      Arr.Add(Key[I]);
    end;
    Json.S['text'] := Key;
    // Json.S['sessionid'] := FSessionID;
    Json.S['id'] := Ele;
    Data := Json.ToJSON();
    // Resp := FCmd.ExecutePost(command, Data);
    Resp := ExecuteCommand(cPost, Command, Data);
    ProcResponse(Resp);
  finally
    FreeAndNil(Json);
  end;
end;

procedure TWebDriver.Set_Window_Size(const Width, Height: integer;
  WindowHandle: string = 'current');
var
  Command: string;
  Data: string;
  Resp: string;
  Json :TJsonObject;
begin
  Json :=TJsonObject.Create;
  try
    if W3C then
    begin
      Command := Host + '/session/' + FSessionID + '/window/rect';
      Json.I['width'] := Width;
      Json.I['height'] := Height;
      // Json.S['sessionid'] := FSessionID;
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
    // Resp := FCmd.ExecutePost(command, Data);
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
  Command: string;
begin
  Command := Host + '/session/' + FSessionID + '/cookie';
  // FCmd.ExecuteDelete(command);
  ExecuteCommand(cDelete, Command)
end;

procedure TWebDriver.DeleteCookie(const cookieName: string);
var
  Command: string;
begin
  Command := Host + '/session/' + FSessionID + '/cookie' + '/' + cookieName;
  // FCmd.ExecuteDelete(command);
  ExecuteCommand(cDelete, Command);
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
  Command: string;
  Data: string;
  Resp: string;
  Json :TJsonObject;
begin
  Json :=TJsonObject.Create;
  try
    Command := Host + '/session/' + FSessionID + '/timeouts';
    Json.Clear;
    Json.S['name'] := 'pageLoad';
    Json.I['value'] := Timeout;
    Data := Json.ToJSON(False);
    // Resp := FCmd.ExecutePost(command, Data);
    Resp := ExecuteCommand(cPost, Command, Data);
    ProcResponse(Resp);
  finally
    FreeAndNil(Json);
  end;
end;

function TWebDriver.AddCookie(cookieName, cookieValue: string): string;
var
  Command: string;
  Data: string;
  Resp: string;
  Json, ckJson: TJsonObject;
begin
  //FJson.Clear;
  //ckJson := FJson.O['cookie'];
  Json :=TJsonObject.Create;
  try
    ckJson :=Json.O['cookie'];
    ckJson.S['name'] := cookieName;
    ckJson.S['value'] := cookieValue;
    Command := Host + '/session/' + FSessionID + '/cookie';
    Data := Json.ToJSON(True);
    // Resp := FCmd.ExecutePost(command, Data);
    Resp := ExecuteCommand(cPost, Command, Data);
    ProcResponse(Resp);
  finally
    FreeAndNil(Json);
  end;
end;

function TWebDriver.ExecuteCommand(const CommandType: TCommandType;
  const Command: string; const Param: string = ''): string;
begin
  FErrorMessage :='';
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

function TWebDriver.FindElementByName(const Name: string): string;
begin
  result := FindElement('name', Name);
end;

function TWebDriver.GetDocument: string;
var
  Command: string;
begin
  result := '';
  if FSessionID <> '' then
  begin
    Command := Host + '/session/' + FSessionID + '/source';
    // result := ProcResponse(FCmd.ExecuteGet(command));
    result := ProcResponse(ExecuteCommand(cGet, Command));
  end;
end;

function TWebDriver.GetAllCookieJsonArray: string;
var
  Command: string;
  Resp: string;
begin
  Command := Host + '/session/' + FSessionID + '/cookie';
  // Resp := FCmd.ExecuteGet(command);
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
  I: integer;
  aryJson: TJsonArray;
  tmpJson: TJsonObject;
begin
  // 用标准接口返回是错误,故用此方法
  Command := Host + '/session/' + FSessionID + '/cookie';
  // Resp := FCmd.ExecuteGet(command);
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
  // Resp := FCmd.ExecuteGet(command);
  Resp := ExecuteCommand(cGet, Command);
  result := ProcResponse(Resp);
end;

function TWebDriver.GetDriverIsRunning: Boolean;
begin
  Result := FProcessInfo.hProcess <>0;
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

procedure TWebDriver.SetTimeout(const Value: integer);
begin
  FCmd.Timeout := Value;
end;

procedure TWebDriver.SwitchToFrame(const FrameID: string);
var
  Command: string;
  Data: string;
  Resp: string;
  ElementID: string;
begin
  Command := Host + '/session/' + FSessionID + '/frame';
  ElementID := FindElementByID(FrameID);
  { FJson.Clear;
    FJson.S['id'] := '{"ELEMENT":'+'"'+ElementID+'"'+','+ }
  // '"'+FCurrentElement.GetElementName+'":'+'"'+ElementID+'"}';
  // FJson.S['sessionid'] := FSessionID;
  // Data := FJson.ToJSON();
  Data := '{"id": {"ELEMENT": "' + ElementID + '", "' +
    FCurrentElement.GetElementName + '": "' + ElementID + '"}}';
  Resp := ExecuteCommand(cPost, Command, Data);
  ProcResponse(Resp);
end;

procedure TWebDriver.WaitForLoaded;
var
  I: integer;
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
  Json:TJsonObject;
begin
  Json :=TJsonObject.Create;
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
  Json :TJsonObject;
begin
  Json :=TJsonObject.Create;
  try
    result := '';
    Command := Host + '/session';
    // Resp := FCmd.ExecutePost(command, Phantomjs_PARAM);
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
  FPath := '';
  FBrowserFileName := '';
end;

function TFireFoxDriver.BuildParams: string;
begin
  result := ' --port '+IntToStr(FPort);
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
  Json :TJsonObject;
begin
  Json :=TJsonObject.Create;
  try
    if FBrowserFileName <> '' then
      if not FileExists(FBrowserFileName) then
      begin
        raise Exception.Create('firefox file is not exits.' + FBrowserFileName +
          '.please setup browserfile property');
      end;

    result := '';
    Command := Host + '/session';
    JsStr := ReplaceStr(FBrowserFileName, '\', '\\');
    // Resp := FCmd.ExecutePost(command, Format(Firefox_Param, [JsStr, JsStr]));
    Resp := ExecuteCommand(cPost, Command, format(Firefox_Param, [JsStr, JsStr]));
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

procedure TFireFoxDriver.StartDriver(const ExeName: string; const Args: string
    = '');
var
  Command: string;
begin
  if not FileExists(ExeName) then
    raise Exception.Create('driver file not exists.' + ExeName);
  FDriverName :=ExeName;
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
  Json :TJsonObject;
begin
  Json :=TJsonObject.Create;
  try
  result := '';
  Command := Host + '/session';
  // Resp := FCmd.ExecutePost(command, IE_Param);
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
  FPort := 6666;
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
  Json:TJsonObject;
begin
  Json :=TJsonObject.Create;
  try
  result := '';
  Command := Host + '/session';
  // Resp := FCmd.ExecutePost(command, Chrome_Param);
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
  FDriverName :=ExeName;
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
  Json :TJsonObject;
begin
  Json :=TJsonObject.Create;
  try
    result := '';
    Command := Host + '/session';
    // Resp := FCmd.ExecutePost(command, Edge_Param);
    Resp := ExecuteCommand(cPost, Command, Edge_Param);
    if Resp <> '' then
    begin
      Json.FromJSON(Resp);
      FW3C := not Json.Contains('status');
      if (not Json.Contains('sessionId')) or
      (Json.Values['sessionId'].VariantValue= null) then
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
  ElementID: string;
begin
  Command := Host + '/session/' + FSessionID + '/frame';

  Data := '{"id": "'+frameID+'",'+'"sessionId":"'+FSessionID+'"}';
  Resp := ExecuteCommand(cPost, Command, Data);
  ProcResponse(Resp);
end;

function TElement.GetElementID: string;
var
  Json: TJsonObject;
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

function TElement.GetElementName: string;
var
  Json: TJsonObject;
begin
  if not W3C then
    result := ElementData
  else
  begin
    Json := TJsonObject.Create;
    try
      Json.FromJSON(ElementData);
      result := Json.Names[0];
    finally
      FreeAndNil(Json);
    end;
  end;
end;

end.
