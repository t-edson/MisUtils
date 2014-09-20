MisUtils 0.1b
=============

Useful routines for Lazarus, for showing messages and developing multilingual applications.

Inside this unit, we have this functions for showing messages:

* MsgBox() -> Shows a box with a message. Similar a ShowMessage.
* MsgExc() -> Shows a box with a message and an Icon of Exclamation. 
* MsgErr() -> Shows a box with a message and an Icon of Error. 

* MsgYesNo() -> Prompt for a Yes/No answer.
* MsgYesNoCancel() -> Prompt for a Yes/No answer, with a cancel option.

Moreover, this unit includes a diccionary for trnslating messages. For to use it, are  declared the next fileds:

* TranslateMsgs -> Flag for to enable the translation of  messages used with MsgBox(), MsgExc() and MsgErr().
* dic() -> Translate one string using the internal dictionary.
* dicSet() -> Add or replace an entry of the internal dictionary.
* dicDel() -> Clear an entry of the internal dictionary.

