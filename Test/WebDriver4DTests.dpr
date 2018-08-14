program WebDriver4DTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options 
  to use the console test runner.  Otherwise the GUI test runner will be used by 
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}


uses
  DUnitTestRunner,
  vcl.Forms,
  System.SysUtils,
  TestWebDriver in 'TestWebDriver.pas',
  Webdriver4D in '..\Webdriver4D.pas',
  JsonDataObjects in '..\JsonDataObjects.pas',
  WD_http in '..\WD_http.pas',
  WD_httpDelphi in '..\WD_httpDelphi.pas',
  webdriver_js_string in '..\webdriver_js_string.pas';

{frmSpSettings}


{$R *.RES}






begin
	{$IFDEF DEBUG}
  	ReportMemoryLeaksOnShutdown :=true;
  {$ENDIF}

 

  	DUnitTestRunner.RunRegisteredTests;
 
end.

