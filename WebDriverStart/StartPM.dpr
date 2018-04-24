program StartPM;

uses
  Vcl.Forms,
  frm_WebDrvier_Service in 'frm_WebDrvier_Service.pas' {TWebDriverService},
  JsonDataObjects in '..\JsonDataObjects.pas',
  Webdriver4D in '..\Webdriver4D.pas',
  WD_httpDelphi in '..\WD_httpDelphi.pas',
  WD_http in '..\WD_http.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TTWebDriverService, TWebDriverService);
  Application.Run;
end.
