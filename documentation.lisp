#|
 This file is a part of feeder
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.feeder)

;; protocol.lisp
(docs:define-docs
  (type remote-item
    "An item representing a remote resource of some kind.

See LINK
See URL")
  
  (function link
    "Accessor to the link of the item.

See LINK (type)
See REMOTE-ITEM")
  
  (function url
    "Accessor to the URL of a link or remote item.

The URL should be encoded as a string, and no URL validation is
performed.

See LINK (type)
See REMOTE-ITEM")
  
  (type person
    "Representation of a person.

See REMOTE-ITEM
See NAME
See EMAIL")
  
  (function name
    "Accessor to the name of the item.

See PERSON
See GENERATOR (type)")
  
  (function email
    "Accessor to the email address of the person.

No address validation is performed.

See PERSON")
  
  (type generator
    "Representation of a feed generator.

See REMOTE-ITEM
See NAME
See VERSION")
  
  (function version
    "Accessor to the generator version.

See GENERATOR (type)")
  
  (type link
    "Representation of a link to an external resource.

See URL
See RELATION
See CONTENT-TYPE
See LANGUAGE
See TITLE")
  
  (function relation
    "Accessor to the relation of the link.

The following values are typically recognised:

  - \"alternate\"
  - \"related\"
  - \"self\"
  - \"enclosure\"
  - \"via\"

See RFC4287 for more information.

See LINK (type)")
  
  (function content-type
    "Accessor to the content mime-type at the end of the link.

See LINK (type)")
  
  (function language
    "Accessor to the language of the item

This should be a two or three letter code name of the language in
which the item's content is written, though no validation to this
effect is performed.

See LINK (type)
See AUTHORED-ITEM")
  
  (function title
    "Accessor to the title of the item

This may be a plaintext string or a PLUMP:NODE

See LINK (type)
See AUTHORED-ITEM
See PLUMP:NODE")
  
  (type authored-item
    "Representation of a basic feed item.

This is used as the base class for FEEDs and ENTRYs in a feed.

See REMOTE-ITEM
See ID
See CATEGORIES
See AUTHORS
See CONTRIBUTORS
See PUBLISHED-ON
See UPDATED-ON
See RIGHTS
See LANGUAGE
See LINK
See TITLE
See SUMMARY
See CONTENT")
  
  (function id
    "Accessor to the unique ID of the item.

This should either be a value that can be PRINCed to obtain a string
representation of the unique identifier, or a LINK.

See LINK (type)
See AUTHORED-ITEM")
  
  (function categories
    "Accessor to the list of categories that the item relates to.

Each category should be a simple string.

See AUTHORED-ITEM")
  
  (function authors
    "Accessor to the list of persons that authored the item.

See PERSON
See AUTHORED-ITEM")
  
  (function contributors
    "Accessor to the list of persons that contributed to the item.

See PERSON
See AUTHORED-ITEM")
  
  (function published-on
    "Accessor to the date on which this item was first published.

The date should be a LOCAL-TIME:TIMESTAMP

See LOCAL-TIME:TIMESTAMP
See AUTHORED-ITEM")
  
  (function updated-on
    "Accessor to the date on which this item was last updated.

The date should be a LOCAL-TIME:TIMESTAMP

See LOCAL-TIME:TIMESTAMP
See AUTHORED-ITEM")
  
  (function rights
    "Accessor to copyright information relating to the item.

This may be a plaintext string or a PLUMP:NODE

See AUTHORED-ITEM")
  
  (function summary
    "Accessor to the summary describing the item in short.

This may be a plaintext string or a PLUMP:NODE

In absence of a CONTENT value, it may also represent the full
content.

See AUTHORED-ITEM")
  
  (function content
    "Accessor to the content of the item.

For FEEDs, the content should be a list of ENTRYs.
For ENTRYs this may be a plaintext string or a PLUMP:NODE.

See FEED
See ENTRY
See PLUMP:NODE
See AUTHORED-ITEM")
  
  (type feed
    "Representation of a syndication feed.

See AUTHORED-ITEM
See CACHE-TIME
See GENERATOR
See LOGO
See WEBMASTER")
  
  (function cache-time
    "Accessor to the amount of time the feed can be cached.

This value should be an integer representing the cache time in
minutes.

See FEED")
  
  (function generator
    "Accessor to the generator that created this feed.

See GENERATOR (type)
See FEED")
  
  (function logo
    "Accessor to the logo for the feed

This should be a LINK

See FEED")
  
  (function webmaster
    "Accessor to the webmaster responsible for this feed.

This should be a PERSON

See FEED")
  
  (type entry
    "Representation of a feed item.

See AUTHORED-ITEM
See COMMENT-SECTION
See SOURCE")
  
  (function comment-section
    "Accessor to the link for a comment section for this item.

Should be a LINK or URL.

See LINK (type)
See ENTRY")
  
  (function source
    "Accessor to the source of this item.

This is used if the entry is aggregated from elsewhere.

Should be a LINK.

see LINK (type)
See ENTRY")
  
  (type format
    "Base class for an external format for a feed.

See SOURCE-HAS-FORMAT-P
See PARSE-FEED
See SERIALIZE-FEED
See PARSE-TO
See SERIALIZE-TO")

  (type xml-format
    "Base class for formats based on XML.

See FORMAT")
  
  (function source-has-format-p
    "Returns T if the given source is encoded in the given format

See FORMAT")

  (function instance-for-type
    "Returns an appropriate instance for the requested type under the specified format.

By default this constructs an empty (all slots set to NIL) instance
using the given type as a class name.

See FORMAT")
  
  (function parse-feed
    "Parses the given source into standardised feed objects according to the specified format.

If FORMAT is T, the format is determined automatically depending on
the source's contents.

Returns a list of FEED instances.

This function should construct the appropriate base object and then
call PARSE-TO.

See FEED
See FORMAT")
  
  (function serialize-feed
    "Turns the given feed into the specified format.

Returns the encoded feed.

For XML-FORMATs this will be a PLUMP:NODE

This function should construct the appropriate base object and then
call SERIALIZE-TO.

See PLUMP:NODE
See XML-FORMAT
See FORMAT")
  
  (function parse-to
    "Fills the target with content from thing according to format.

This is used internally in the parsing process.

See FORMAT")
  
  (function serialize-to
    "Fills the target with content from thing according to format.

This is used internally in the serialisation process.

See FORMAT"))

;; rss.lisp
(docs:define-docs
  (type rss
    "RSS 2.0 Feed Format

As defined in https://validator.w3.org/feed/docs/rss2.html with select
extensions such as content:encoded.

RSS is a very \"web\" format, which is to say that the files claiming
to be RSS that can be found out there all do not adhere to any strict
specifications, or anything at all for that matter. This makes parsing
and dealing with RSS a pain in the ass.

This parser performs a best-effort at parsing the content you hand it,
and attempts to standardise and culminate certain features together as
appropriate. It should not error when parsing a feed, but may miss or
misinterpret certain values present in the raw feed data. For
instance, of duplicated tags that should only exist once, only the
last tag is actually preserved in the generated objects.

Serialising to RSS will follow the description as closely as
possible without touching undefined parts.

See XML-FORMAT"))

;; atom.lisp
(docs:define-docs
  (type unknown-atom-content-type
    "Error signalled on an unknown content type.

When this condition is signalled, the following restarts are available

  - USE-TYPE
    Requires an argument that specifies the alternate content-type to
    use.
  - USE-VALUE
    Requires an argument that specifies the content to use in its
    place.
  - TREAT-AS-PLAINTEXT
    Treats the content as plaintext and returns it.
  - CONTINUE
    Ignores the content and returns NIL

See FEED-CONDITION
See ATOM")
  
  (type atom
    "Atom Feed Format

As defined in RFC4287 https://tools.ietf.org/html/rfc4287 .

As opposed to RSS, Atom is rather strict, and as such parsing of the
feed data does not attempt to guess either. However, just as with RSS,
parsing a feed should not signal an error.

See XML-FORMAT"))

;; toolkit.lisp
(docs:define-docs
  (type feed-condition
    "Base condition for all feed related conditions.

See ARGUMENT-MISSING
See NIL-VALUE
See UNKNOWN-FORMAT
See UNKNOWN-ATOM-CONTENT-TYPE")
  
  (type argument-missing
    "Error signalled when a required argument is missing.

This usually happens when you try to create an instance of an object
but omitted an argument that is required to create a valid feed.

When this condition is signalled, the following restarts are available

  - USE-VALUE
    Requires an argument that is then used for the missing argument.
  - CONTINUE
    Set the slot to NIL anyway.

See FEED-CONDITION")
  
  (type nil-value
    "Error signalled when a form returns NIL that should not be NIL.

This usually happens during feed serialisation when a slot is empty
that is required to generate a valid feed.

When this condition is signalled, the following restarts are available

  - USE-VALUE
    Requires an argument that is then returned in place of the NIL.

See FEED-CONDITION")
  
  (type unknown-format
    "Error signalled when the given feed source has an unknown format.

When this condition is signalled, the following restarts are available

  - USE-VALUE
    Requires an argument that designates the format to use.

See FEED-CONDITION
See PARSE-FEED")

  (function make-element
    "Construct a new XML element.

ATTRIBUTES should be a plist of alternating keys and values. A key may
either be a string or a symbol. If a symbol, it is treated as the
attribute name in lowercase. The attribute is only set if the value is
non-NIL. 

If the key is a symbol with the name \"-\", the value is used as the
text content of the element.

For example, constructing an element like

   <foo bar=\"baz\">bam</foo>

would be

  (make-element parent \"foo\"
    :bar \"baz\"
    - \"bam\")

See PLUMP:MAKE-ELEMENT
See PLUMP:ATTRIBUTE
See PLUMP:MAKE-TEXT-NODE")

  (function with-children
    "Scans through the immediate children of ROOT and executes bodies as matching.

The format should be as follows:

  CLAUSES ::= (TAG . form*)*
  TAG     --- A string designator

The forms of a clause are executed with NAME bound to a PLUMP:ELEMENT
whose tag-name matches that specified in the clause. Tag names are
matched case-insensitively.")

  (function text
    "Returns the trimmed text contents of the given node.

This is like PLUMP:TEXT, but with ASCII whitespace trimmed off the
front and end of the text.

See PLUMP:TEXT"))
