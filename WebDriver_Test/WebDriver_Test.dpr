program WebDriver_Test;

uses
  Vcl.Forms,
  frm_main in 'frm_main.pas' {Form1},
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
