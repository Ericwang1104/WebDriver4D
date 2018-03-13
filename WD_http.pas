unit WD_http;

interface

uses
  Classes, Sysutils, System.Net.HttpClient, System.Net.URLClient,
  System.NetConsts;

type

  TDriverCommand = class(TComponent)
  private
    FTimeout: integer;
  protected
    FSTM: TStringStream;
    procedure InitHeader; virtual; abstract;
    function GetTimeout: integer; virtual; abstract;
    procedure SetTimeout(const Value: integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteDelete(command: string); virtual; abstract;
    function ExecuteGet(URL: string): string; virtual; abstract;
    function ExecutePost(const URL, Data: string): string; virtual; abstract;
    property Timeout: integer read GetTimeout write SetTimeout;
  end;

implementation

constructor TDriverCommand.Create(AOwner: TComponent);
begin
  inherited;
  FSTM := TStringStream.Create('', TEncoding.UTF8);
  FTimeout := 6000;
end;

destructor TDriverCommand.Destroy;
begin
  FreeAndNil(FSTM);
  inherited;
end;

end.
