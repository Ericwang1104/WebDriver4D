program StartPM;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  delphi_driver in '..\delphi_driver.pas',
  JsonDataObjects in '..\JsonDataObjects.pas',
  WD_http in '..\WD_http.pas',
  WD_httpDelphi in '..\WD_httpDelphi.pas',
  Webdriver4D in '..\Webdriver4D.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
