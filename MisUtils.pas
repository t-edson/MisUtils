{MisUtils 0.1b
 =============
 Por Tito Hinostroza 19/09/2014
 * Se agrega soporte para internacionalización, agregando un diccionario.

 Descripción
 ============
 Librería de funciones útiles para mostrar mensajes en pantalla, para guardar datos en
 archivos, para crear aplicaciones en variso idiomas y algunas utilidades adicionales.
 }
unit MisUtils;

{$mode objfpc}{$H+}

interface

uses  Classes, SysUtils, Forms, Graphics, Dialogs, process, Controls,
      lclType, FileUtil, types, dateutils;

var
  msjError  : string;       //mensaje de error de la aplicación
  dictionary: TstringList;  //diccionario para el manejo de mensajes
  TranslateMsgs: boolean;   //activa la traducción del mensaje

procedure MsgExc(txt: string);
procedure MsgErr(txt: string);
//function MsgBox(txt: PChar; Caption: string = ''; flags: longint = 0): integer;
function MsgBox(txt: String; Caption: string = ''; flags: longint = 0): integer;
procedure MsgBox(Fmt : String; const Args : Array of const);
function MsgYesNo(txt: string): byte;
function MsgYesNoCancel(txt: string): byte;

function Explode(delimiter:string; str:string; limit:integer=MaxInt):TStringDynArray;
function Exec(com: string): boolean;
procedure StringToFile(const s: string; const FileName: string);
function StringFromFile(const FileName: string): string;

Function f2N(s : String): Double;
Function B2f(b : Boolean) : String;
Function f2B(s : String) : Boolean;
Function D2f(d : TDateTime): String;
Function f2D(s : String) : TDateTime;

//Funciones del diccionario
procedure dicSet(key, value: string);  //fija una entrada del diccionario
procedure dicDel(key: string);  //limpia una entrada del diccionario
procedure TransCapCtrls(TheForm: TForm; Caption, value: string);  //traduce un mensaje de un control
function dic(key: string): string;     //lee un mensaje traducido

implementation

const szChar = SizeOf(Char);

procedure MsgExc(txt: string);
//Mensaje de exclamación
begin
  if TranslateMsgs then txt := dic(txt);
  Application.MessageBox(PChar(txt), '', MB_ICONEXCLAMATION);
end;
procedure MsgErr(txt: string);
//Mensaje de error
begin
  if TranslateMsgs then txt := dic(txt);
  Application.MessageBox(PChar(txt), '', MB_ICONERROR);
end;
{function MsgBox(txt: PChar; Caption: string = ''; flags: longint = 0): integer;
begin
  if TranslateMsgs then txt := dic(txt);
  Result := Application.MessageBox(txt, PChar(Caption), flags);
end;}
function MsgBox(txt: String; Caption: string = ''; flags: longint = 0): integer;
begin
  if TranslateMsgs then txt := dic(txt);
  Result := Application.MessageBox(Pchar(txt), PChar(Caption), flags);
end;
procedure MsgBox(Fmt: String; const Args: array of const);
var
  txt: String;
begin
  if TranslateMsgs then Fmt := dic(Fmt);
  txt := Format(Fmt, Args);
  Application.MessageBox(Pchar(txt), '', 0);
end;

function MsgYesNo(txt: string): byte;
//Muestra un mensaje en pantalla con los botones Yes - No
//Devuelve 1, si para la opción Yes
//Devuelve 2, si para la opción No
var
  r: Integer;
begin
  Result := 0;  //Valor por defecto
  r := Application.MessageBox(PChar(txt),'',MB_YESNO + MB_ICONQUESTION);
  if r = IDYES then exit(1);
  if r = IDNO  then exit(2);
end;

function MsgYesNoCancel(txt: string): byte;
//Muestra un mensaje en pantalla con los botones Yes - No - Cancel
//Devuelve 1, si para la opción Yes
//Devuelve 2, si para la opción No
//Devuelve 3, si para la opción Cancel
var
  r: Integer;
begin
  Result := 0;  //Valor por defecto
  r := Application.MessageBox(PChar(txt),'',MB_YESNOCANCEL + MB_ICONQUESTION);
  if r = IDYES then exit(1);
  if r = IDNO  then exit(2);
  if r = IDCANCEL  then exit(3);
end;

function Explode(delimiter:string; str:string; limit:integer=MaxInt):TStringDynArray;
var
  p,cc,dsize:integer;
begin
  cc := 0;
  dsize := length(delimiter);
  while cc+1 < limit do begin
    p := pos(delimiter,str);
    if p > 0 then begin
      inc(cc);
      setlength(result,cc);
      result[cc-1] := copy(str,1,p-1);
      delete(str,1,p+dsize-1);
    end else break;
  end;
  inc(cc);
  setlength(result,cc);
  result[cc-1] := str;
end;

function Exec(com: string): boolean;
//Ejecuta un programa. Devuelve FALSE si hubo error
var
  p    : TProcess;   //el proceso a manejar
begin
  Result := true;
  p := TProcess.Create(nil); //Crea proceso
//  p.CommandLine := SysToUTF8(Application.ExeName +  arc0 + parMsg);
  p.CommandLine := SysToUTF8(com);
  try
    p.Execute;
  except
    Result := false;
    MsgBox('Fallo al iniciar aplicativo: '+ p.CommandLine);;
  end;
  p.Free;
end;

procedure StringToFile(const s: string; const FileName: string);
///   saves a string to a file
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    FileStream.WriteBuffer(Pointer(s)^, (Length(s) * szChar));
  finally
    FreeAndNil(FileStream);
  end; // try
end;
function StringFromFile(const FileName: string): string;
///   returns the content of the file as a string
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Result, (FileStream.Size div szChar));
    FileStream.ReadBuffer(Pointer(Result)^, FileStream.Size);
  finally
    FreeAndNil(FileStream);
  end; // try
end;

//############## Funciones de conversión de datos para acceso a disco ############
{Function N2f(n As Single):String;
//Convierte número a cadena para guardar en disco. Independiente de la configuración regional
begin
    N2f = Replace(CStr(n), ",", ".")    //asegura que se usa siempre el punto decimal
End;
}
function f2N(s: String): Double;
//Convierte cadena de disco a número. Independiente de la configuración regional
begin
    Result := StrToFloat(s);     //usa siempre el punto decimal
End;

function B2f(b: Boolean): String;
//Convierte Boleean a cadena para guardar en disco.
begin
    If b Then Result := 'V' Else Result := 'F';
End;

function f2B(s: String): Boolean;
//Convierte cadena de disco a Boleean
begin
    If s = 'V' Then exit(True) else exit(False);
End;

function D2f(d: TDateTime): String;
//Convierte fecha a cadena para guardar en disco.
var
  s: string;
begin
  DateTimeToString(s,'yyyy:mm:dd:hh:nn:ss',d);
  Result :=  s;
End;

function f2D(s: String): TDateTime;
//Convierte cadena de disco a fecha.
var a: TStringDynArray;
begin
//    If (s = '0') Or (s = '') Then Exit Function   //para proteger de las versiones anteriores
  a := explode(':',s);
  Result := EncodeDateTime(StrToInt(a[0]), StrToInt(a[1]), StrToInt(a[2]),
                           StrToInt(a[3]), StrToInt(a[4]), StrToInt(a[5]), 0);
End;

{
Function S2f(s : String) : String;
//Convierte cadena a formato para guardar en disco.
var tmp : String;
begin
    tmp = Replace(s, vbTab, "\t")   'no se permiten tabulaciones
    S2f = Replace(s, vbCrLf, "\n")  'tampoco saltos de línea
End;

Function f2S(s : String) : String;
//Convierte cadena de disco a cadena.
var tmp : String;
begin
    tmp = Replace(s, "\t", vbTab)   'recupera tabulaciones
    f2S = Replace(s, "\n", vbCrLf)  'y saltos de línea
End;
}

procedure dicSet(key, value: string);
//Fija o agrega una entrada al diccionario
begin
  dictionary.values[key]:=value;
end;

procedure dicDel(key: string);
//Limpia una entrada del diccionario
begin
  dictionary.values[key]:='';
end;

procedure TransCapCtrls(TheForm: TForm; Caption, value: string);
//Traduce la etiqueta de un control de un formulario
var
  c: TControl;
  i : integer;
begin
   for i := 0 to TheForm.ControlCount-1 do begin
     c := theForm.Controls[i];
     if c.Caption = Caption then c.Caption := value;
   end;
end;

function dic(key: string): string;
//Devuelve un mensaje en el lenguaje definido, dada la clave.
//La clave no puede tener el signo "="
begin
  Result := dictionary.Values[key];
  //si no enecuentra, devuelve la misma clave
  if Result = '' then Result := key;
end;

Initialization
  //crea diccionario
  dictionary := TStringList.Create;
  TranslateMsgs := false;
Finalization

  dictionary.Destroy;
end.

