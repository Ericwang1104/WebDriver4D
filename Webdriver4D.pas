unit Webdriver4D;

interface

uses
  Classes, SysUtils, Windows,  Vcl.Graphics,
  JsonDataObjects, Winapi.ShlObj;

type
  TPCharStack=record
  private
    StackArr :array of Pchar;
    FPos:integer;
    function GetCount: Integer;
  public

    constructor Create(StackCount: Integer);
    procedure Push(chr:PChar);
    function Pop: PChar;
    property Count: Integer read GetCount;

  end;
  TDriverCommand = class(TComponent)
  private
    FTimeout: Integer;
  strict protected
    procedure InitHeader; virtual; abstract;
  protected
    FSTM: TStringStream;
    function GetTimeout: Integer; virtual; abstract;
    procedure SetTimeout(const Value: Integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure ExecuteDelete(command: string); virtual; abstract;
    function ExecuteGet(URL: string): string; virtual; abstract;
    function ExecutePost(const URL, Data: string): string; virtual; abstract;
    property Timeout: Integer read GetTimeout write SetTimeout;
  end;

type
  TDriverType = (btPhantomjs, btIE, btFirefox, btChrome);

  TWebDriver = class(TComponent)
  strict private
    Fcapabilities: string;
    FPort: Integer;
    FSessionID: string;
    function BuildParams: string;
    procedure CutImage(const FileName: string; Pic: string; X, Y, Width, Height: Integer);
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
    function GetTimeOut: Integer;
    procedure SaveScreenToFileName(const FileName, Base64File: string);
    procedure SetBrowserType(const Value: TDriverType);
    procedure SetTimeOut(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartPm(const ExeName: string);
    procedure Assign(Source: TPersistent); override;
    procedure CloseWindow(ParamSessionID: string = '');
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
    function NewSession(BrowserFileName: string = ''): string;
    procedure Save_screenshot(const FileName: string);
    procedure Set_Window_Size(const Width, Height: Integer; WindowHandle: string = 'current');
    procedure DeleteSession(ParamSessionID: string = '');
    procedure ElementClick(const Element: string);
    function Element_Location(const Element: string): string;
    procedure Element_ScreenShort(const Element, FileName: string);
    function Element_Size(const Element: string): string;
    function ExecuteScript(const Script: string; const Args: string = '[]'): string;
    function FindElementsByXPath(XPath: string): string;
    function FindElementsByTag(const TagName: string): string;
    function FindElementsByLinkText(const LinkText: string): string;
    function FindElementsByID(const ID: string): string;
    function FindElementsByClassName(const ClasName: string): string;
    function GetAllSession: string;
    function Get_AllCookies: string;
    procedure Implicitly_Wait(const waitTime: Double);
    procedure PageLoadTimeout(const Timeout: Integer);
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
    property Port: Integer read FPort write FPort;
    property Path: string read FPath write FPath;
    property SessionID: string read FSessionID write FSessionID;
  published
    procedure Clear;
    property Timeout: Integer read GetTimeOut write SetTimeOut;
  end;

function MatchString(const PS: Pchar; const KeyWord: string): Boolean;

function JsonSimpleParser(const KeyName, Data: string): string;

implementation

uses
  System.NetEncoding, Vcl.Imaging.pngimage, System.StrUtils, Winapi.TlHelp32;

function MatchString(const PS: Pchar; const KeyWord: string): Boolean;
var
  strBuf: string;
  Len: integer;
begin
  Len := Length(KeyWord);
  SetString(strBuf, PS, Len);
  result := SameText(strBuf, KeyWord);

end;

function JsonSimpleParser(const KeyName, Data: string): string;
var
  I: Integer;
  PS, PE: Pchar;
  PElementS, PElementE: Pchar;
  KeyWord: string;
  FindKey: Boolean;
  Stack:TPCharStack;
begin
  PS := @Data[Low(Data)];
  PE := @Data[High(Data)];
  KeyWord := '"' + KeyName + '"';
  FindKey := false;
  Stack :=TPcharStack.Create(256);
  while PS < PE do
  begin
    if not FindKey then
    begin
      if MatchString(PS, KeyWord) then
      begin

      end;
      Inc(PS);
    end
    else
    begin
      if PS^ = ':' then
      begin
        PElementS :=PS+1;
      end
      else if PS^ = '{' then
      begin
        // push stack
        Stack.Push(PS);
      end
      else if PS^ = '}' then
      begin
        //pop stack
        if Stack.Count=0 then
        begin
          PElementE :=PS;
          SetString(result,PElementS,PElementE-PElementS+1);
          Exit;
        end;


      end;
    end;
    Inc(PS);

  end;

end;

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
end;

destructor TWebDriver.Destroy;
begin
  FreeAndNil(FJson);
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

procedure TWebDriver.CutImage(const FileName: string; Pic: string; X, Y, Width, Height: Integer);
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
  FJson.FromJSON(Resp);
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
  FJson.FromJSON(Resp);
  result := ProcResponse(Resp);
end;

function TWebDriver.ExecuteScript(const Script: string; const Args: string = '[]'): string;
var
  command: string;
  Data: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/execute';
  FJson.Clear;
  FJson.S['script'] :=Script;
  FJson.S['sessionid'] :=FSessionID;
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

  FJson.S['value'] :=KeyName;
  FJson.S['sessionid'] := FSessionID;
  FJson.S['using'] :=usingName;
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

function TWebDriver.GetElementAttribute(const Element, attributename: string): string;
var
  command: string;
  Ele: string;
  Resp: string;
begin
  FJson.FromJSON(PChar(Element));
  Ele := FJson.S['ELEMENT'];
  command := Host + '/session/' + FSessionID + '/element/' + Ele + '/attribute/' + attributename;
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
  i: Integer;
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
    i := 0;
    while i < Timeout do
    begin
      sleep(500);
      inc(i, 1000);
      if ExecuteScript('return document.readyState') = 'complete' then
        break;
    end;
  end;
end;

function TWebDriver.Get_AllCookies: string;
var
  command: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/cookie';
  Resp := FCmd.ExecuteGet(command);
  FJson.FromJSON(Resp);
  result := ProcResponse(Resp);
end;

procedure TWebDriver.Implicitly_Wait(const waitTime: Double);
var
  command: string;
  Data: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/timeouts/implicit_wait';
  FJson.Clear;
  FJson.F['ms'] :=waitTime;
  FJson.S['session'] :=FSessionID;
  Data := FJson.ToJSON();
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);
end;

function TWebDriver.NewSession(BrowserFileName: string = ''): string;
const
  Phantomjs_PARAM = '{"desiredCapabilities": {"takesScreenshot":false,"browserName":"phantomjs",' + '"phantomjs.page.settings.userAgent": "Mozilla/5.0 (Windows NT 6.2; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1667.0 Safari/537.36"' + ' , "platform": "windows", "version": "", "javascriptEnabled": true},' + '"capabilities": {"takesScreenshot":false,"browserName": "phantomjs", "firstMatch": [],' + '"phantomjs.page.settings.userAgent": "Mozilla/5.0 (Windows NT 6.2; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1667.0 Safari/537.36"' + ',"platform": "windows", "alwaysMatch": {}, "javascriptEnabled": true, "version": ""}}';
  IE_Param = '{"capabilities": {"firstMatch": [], "alwaysMatch": {"browserName": "internet explorer", "version": "", "platform": "WINDOWS"}}, "desiredCapabilities": {"browserName": "internet explorer", "version": "", "platform": "WINDOWS"}}';
  Firefox_Param = '{"capabilities": {"firstMatch": [], "alwaysMatch": {"browserName": "firefox", ' + '"acceptInsecureCerts": true, "moz:firefoxOptions": ' + '{"binary": "%s"}}}, "desiredCapabilities": {"browserName": "firefox", "acceptInsecureCerts": true, "moz:firefoxOptions": {"binary": "%s"}}}';
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
  FJson.S['sessionid'] :=FSessionID;
  Data := FJson.ToJSON();
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);
  FJson.FromJSON(Resp);
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
    exit;
  FillChar(FStartupInfo, SizeOf(FStartupInfo), 0);
  FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
  FStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  FStartupInfo.wShowWindow := SW_HIDE;
  command := self.BuildParams;

  if CreateProcess(PChar(ExeName), PChar(command), nil, nil, false, NORMAL_PRIORITY_CLASS, nil, nil, FStartupInfo, FProcessInfo) then
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
  X, Y, Width, Height: Integer;
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
  FJson.S['value'] :=KeyName;
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
  i: Integer;
begin
  FJson.FromJSON(Element);
  Ele := FJson.S['ELEMENT'];
  command := Host + '/session/' + FSessionID + '/element/' + Ele + '/value';

  FJson.Clear;

  KeyArr := '[';
  for i := 1 to Length(Key) do
  begin
    if KeyArr = '[' then
      KeyArr := KeyArr + '"' + Key[i] + '"'
    else
      KeyArr := KeyArr + ',' + '"' + Key[i] + '"';
  end;

  KeyArr := KeyArr + ']';

  FJson.A['value'].FromJSON( KeyArr);
  FJson.S['text'] :=Key;
  FJson.S['sessionid'] :=FSessionID;
  FJson.S['id'] :=Ele;
  Data := FJson.ToJSON();
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);
end;

procedure TWebDriver.Set_Window_Size(const Width, Height: Integer; WindowHandle: string = 'current');
var
  command: string;
  Data: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/window/' + WindowHandle + '/size';
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
  i: Integer;
begin
  AllSession := GetAllSession;
  Json := TJsonArray.Create;
  try
    if AllSession <> '' then
    begin
      Json.FromJSON(AllSession);
      for i := 0 to Json.Count - 1 do
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

function TWebDriver.GetTimeOut: Integer;
begin

  result := FCmd.Timeout;
end;

procedure TWebDriver.PageLoadTimeout(const Timeout: Integer);
var
  command: string;
  Data: string;
  Resp: string;
begin
  command := Host + '/session/' + FSessionID + '/timeouts';
  FJson.Clear;
  FJson.S['type'] :='page load';
  FJson.S['sessionid'] := FSessionID;
  FJson.I['ms'] := Timeout;
  Data := FJson.ToJSON(false);
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);
end;

function TWebDriver.ProcResponse(const Resp: string): string;
var
  JSon,Obj: TJsonObject;
  jType:TJsonDataType;

begin
  JSon := TJsonObject.Create;
  try
    if Resp <> '' then
    begin
      JSon.FromJSON(Resp);
      if JSon.S['status']  = '0' then
      begin
        // success
        FHasError := false;
        jType :=json.Types['value'];
        case jType of
          jdtString, jdtInt, jdtLong, jdtULong, jdtFloat, jdtDateTime, jdtBool:
          begin
            Result :=JSon.S['value'];
          end;
          jdtObject:
          begin
            Obj :=JSon.O['value'];
            if Assigned(Obj) then
              result :=Obj.ToJSON()
            else
              Result :='';

          end;
          jdtArray:
          begin
            result :=JSon.A['value'].ToJSON();
          end;
        else
          result :=json.S['value'];
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
    FreeAndNil(JSon);
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

procedure TWebDriver.SetTimeOut(const Value: Integer);
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
  FJson.S['id'] :=FrameID;
  FJson.S['sessionid'] :=FSessionID;
  Data := FJson.ToJSON();
  Resp := FCmd.ExecutePost(command, Data);
  ProcResponse(Resp);

end;

constructor TDriverCommand.Create(AOwner: TComponent);
begin
  inherited;
  FSTM := TStringStream.Create('', TEncoding.UTF8);
  FTimeout :=6000;
end;

destructor TDriverCommand.Destroy;
begin
  FreeAndNil(FSTM);
  inherited;
end;

constructor TPCharStack.Create(StackCount: Integer );
begin
  inherited;
  SetLength(StackArr,Stackcount);
  FPos :=0;
end;

function TPCharStack.GetCount: Integer;
begin
  Result :=FPos ;
end;

function TPCharStack.Pop: PChar;
begin
  if FPos=0 then
  begin
    result :=nil;
  end else
  begin
    result :=StackArr[FPos];
    StackArr[FPos] :=nil;
    Dec(FPos);
  end;
end;

procedure TPCharStack.Push(chr:PChar);
begin
  if FPos>High(FPos) then
  begin
    SetLength(StackArr,Round(High(StackArr)*0.2));
  end;
  StackArr[FPos+1] :=chr;
  Inc(FPos);

end;

end.

