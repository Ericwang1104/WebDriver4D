unit WD_httpDelphi;

interface

uses
  Classes, Sysutils, System.Net.HttpClient, System.Net.URLClient,
  System.NetConsts, WD_http;

type
  TDelphiCommand = class(TDriverCommand)
  private
    Fhttp: THTTPClient;
  protected
    procedure InitHeader; override;
    function GetTimeout: integer; override;
    procedure SetTimeout(const Value: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteDelete(command: string); override;
    function ExecuteGet(URL: string): string; override;
    function ExecutePost(const URL, Data: string): string; override;
  end;

implementation

constructor TDelphiCommand.Create(AOwner: TComponent);
begin
  inherited;
  Fhttp := THTTPClient.Create;
end;

destructor TDelphiCommand.Destroy;
begin
  FreeAndNil(Fhttp);
  inherited;
end;

procedure TDelphiCommand.ExecuteDelete(command: string);
begin
  try
    Fhttp.Delete(command);
  except
    //
  end;
end;

function TDelphiCommand.ExecuteGet(URL: string): string;
begin
  result := '';
  try
    InitHeader;
    FSTM.Clear;
    Fhttp.Get(URL, FSTM);
    result := FSTM.DataString;
  except
    result := FSTM.DataString;
  end;
end;

function TDelphiCommand.ExecutePost(const URL, Data: string): string;
var
  PostStream: TStringStream;
begin
  result := '';
  PostStream := TStringStream.Create('', TEncoding.UTF8);
  try
    try
      PostStream.WriteString(Data);
      PostStream.Position := 0;
      InitHeader;
      FSTM.Clear;
      Fhttp.Post(URL, PostStream, FSTM);
      result := FSTM.DataString;
    finally
      FreeAndNil(PostStream);
    end;
  except
    //
  end;
end;

function TDelphiCommand.GetTimeout: integer;
begin
  result := Fhttp.ResponseTimeout;
end;

procedure TDelphiCommand.InitHeader;
begin
  Fhttp.UserAgent := 'Delphi http Client';
  Fhttp.ContentType := 'application/json';
  Fhttp.Accept := '*/*';
end;

procedure TDelphiCommand.SetTimeout(const Value: integer);
begin
  Fhttp.ResponseTimeout := Value;
end;

end.
