unit delphi_driver;

interface

uses
  Classes,Sysutils, System.Net.HttpClient, Webdriver4D;

type
  TDelphiCommand = class(TDriverCommand)
  private
    Fhttp: THTTPClient;
    procedure InitHeader; override;
  protected
    function GetTimeout: Integer; override;
    procedure SetTimeout(const Value: Integer); override;
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
  Fhttp :=THTTPClient.Create;
end;

destructor TDelphiCommand.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

procedure TDelphiCommand.ExecuteDelete(command: string);
begin
  try
    FHttp.Delete(command);
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
    FHttp.Get(URL, FSTM);
    result := FSTM.DataString;
  except
    //
    result := FSTM.DataString;
  end;
end;

function TDelphiCommand.ExecutePost(const URL, Data: string): string;
var
  PostStream:TStringStream;
begin
  result := '';
  PostStream :=TStringStream.Create('', TEncoding.UTF8);
  try
    try
      PostStream.WriteString(Data);
      PostStream.Position :=0;
      InitHeader;
      FSTM.Clear;
      FHttp.Post(URL,PostStream,FSTM);
      result := FSTM.DataString;
    finally
      FreeAndNil(PostStream);
    end;
  except
    //
  end;
end;

function TDelphiCommand.GetTimeout: Integer;
begin
  Result := Fhttp.ResponseTimeout;
end;

procedure TDelphiCommand.InitHeader;
begin
  Fhttp.UserAgent :='Delphi http Client';
  Fhttp.ContentType :='application/json';
  Fhttp.Accept :='*/*';
end;

procedure TDelphiCommand.SetTimeout(const Value: Integer);
begin
  Fhttp.ResponseTimeout :=Value;
end;

end.
