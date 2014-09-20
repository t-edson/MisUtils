unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MisUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure Button1Click(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
  private
    procedure SetLanguage(lang: string);
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.RadioButton1Change(Sender: TObject);
begin
  SetLanguage('en');
end;

procedure TForm1.RadioButton2Change(Sender: TObject);
begin
  SetLanguage('es');
end;

procedure TForm1.SetLanguage(lang: string);
begin
  case lang of
  'en': begin
    Button1.Caption := 'Send';
    Label1.Caption := 'Name:';
    //Update messages
    dicDel('Hello');  //it's yet in English
  end;
  'es': begin
    Button1.Caption := 'Enviar';
    Label1.Caption := 'Nombre:';
    //Update messages
    dicSet('Hello','Hola');
//    dicSet('Hello %s','Hola %s');  //alternative way
  end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage(dic('Hello')+ ' ' +Edit1.Text);
//  MsgBox('Hello %s', [Edit1.Text]);  //alternative way, setting TranslateMsgs = TRUE
end;

end.

