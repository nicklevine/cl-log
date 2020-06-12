# cl-log
Common Lisp general purpose logging utility

CL-LOG is a general purpose logging utility, loosely modelled in some respects after Gary King's Log5.

Its features include:

    logging to several destinations at once, via "messengers",
    each messenger is tailored to accept some log messages and reject others, and this tailoring can be changed on-the-fly,
    very rapid processing of messages which are rejected by all messengers,
    fully independent use of the utility by several different sub-systems in an application,
    support for messengers which cl:format text to a stream,
    support for messengers which do not invoke cl:format,
    timestamps in theory accurate to internal-time-units-per-second.

Typically, logging utilities only support the one, simple paradigm of formatting objects to a text-stream. Removing this restriction allows you to:

    log random objects instead of plain text,
    generate internationalized log messages,
    create specialised binary logs,
    base the test-suite of complex applications around logging.

Logging is controlled by the following classes: log-manager, base-messenger, base-message, category-set; any of these might be specialised by your application. Whether or not a messenger will accept a given message is controlled by the "category" of the message and the "filter" of the messenger; these are logical combinations of atoms (typically keywords).

I'd expect CL-LOG to work as-is on any conforming implementation of Common Lisp. I've tested it on: ABCL, ACL, CCL, LW, SBCL.

CL-LOG is released with an MIT-style license.
