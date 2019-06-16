PHP Mode for GNU Emacs
======================

[![Emacs: 26.2](https://img.shields.io/badge/Emacs-26.2-blue.svg)](https://www.gnu.org/software/emacs/)
[![lang: PHP 7](https://img.shields.io/badge/lang-PHP%207-brightgreen.svg)](http://php.net/manual/migration70.php)
[![lang: PHP 5](https://img.shields.io/badge/lang-PHP%205-green.svg)](http://php.net/downloads.php)
[![travis badge][travis-badge]][travis-link]
[![melpa badge][melpa-badge]][melpa-link]
[![melpa stable badge][melpa-stable-badge]][melpa-stable-link]
[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)

This is a major mode development project to support PHP coding in GNU Emacs.
This fork builds on the work of:

1. Turadg Aleahmad (Original Author)
2. Aaron S. Hawley
3. Lennart Borgman
4. Eric James Michael Ritz
5. Syohei Yoshida

All contributors listed below improved PHP Mode as well.

The current maintainer is:

1. USAMI Kenta (@zonuexe)

Please submit any bug reports or feature requests by creating issues on [the GitHub page for PHP Mode][php-mode].  Alternatively you may also request features via [the FeatHub page][feathub] for the entire [PHP suite for GNU Emacs][php-suite].

Installation
------------

**PHP Mode works on Emacs 24.3 or later.**  PHP Mode may work with older versions of Emacs but this is not guaranteed.  Bug reports for problems related to using PHP Mode with older versions of Emacs will most like *not* be addressed.

The current support policy can be found on the [Supported Version] page.

### **(RECOMMENDED)** Install from MELPA

[![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

With GNU Emacs 24 or later then you can use its [package][] feature (or [Cask][]) to install PHP Mode from [MELPA][] or [MELPA Stable][].

### Manual installation

Please `git clone` this project or download and unarchive tar or zip file from [php-mode releases].

You can choose one of the following **A**, **B**, **C** installation methods.

#### **A**: `(load php-mode-autoloads.el)` *(RECOMMENDED)*

This is an initialization method that achieves the same performance and ease of use as using a package manager.

By moving the downloaded file to the extracted path of the current directory and executing the `make` command, byte compilation and `php-mode-autoloads.el` is generated.  Just load the file from `init.el` and you are ready to use.

```el
;; Put follow code into init.el
(when (file-directory-p "~/path/to/php-mode")
  (load "~/path/to/php-mode/php-mode-autoloads.el"))

;; Any code below is *unnecessary*
;; (require 'php-mode)
;; (add-to-list 'load-path (expand-file-name "~/path/to/php-mode"))
;; (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
```
#### **B**: `(autoload 'php-mode)`

This is for advanced users who want to reduce the slight increase in reading when Emacs starts.

Also in this case, it is recommended to byte compile with `make`.

```el
;; Put follow code into init.el
(autoload 'php-mode (expand-file-name "~/path/to/php-mode/php-mode") "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(?:php\\|phtml\\)\\'" . php-mode))

;; Any code below is *unnecessary*
;; (add-to-list 'load-path (expand-file-name "~/path/to/php-mode"))
;; (require 'php-mode)
```

#### **C**: `(require 'php-mode)` *(NOT RECOMMENDED)*

Load `php-mode` synchronously from a specific path.  It will load 10 times the size of the code compared to method **A**, and how much the startup time will depend on the performance of your machine's CPU and file system.

```el
(require 'php-mode "~/path/to/php-mode/php-mode")
(add-to-list 'load-path (expand-file-name "~/path/to/php-mode"))
```

Reporting Bugs
--------------

When reporting a bug please run the function `M-x php-mode-debug` and include its output in your bug report.  This helps up reproduce any problem you may have.


Experimental and In-Progress Features
-------------------------------------

### CC Mode, CEDET, EDE, and Semantic

In 2013 Daniel Haxney began rewriting parts of PHP Mode in terms of Emacs' built-in CC Mode.  This laid the foundation for incorporating some of the inherit IDE-ish features of Emacs, such as CEDET, EDE, and Semantic.  Support for these tools continues to improve thanks to the work of Andrea Turso, Steven Rémot, Joris Steyn, and others.  If you wish to test, contribute to, or simply experiment with such features then [this thread is a good place to start](https://github.com/emacs-php/php-mode/issues/256).

### PHP 7 Support

PHP 7 has been released.  PHP Mode supports the following features and changes from PHP 7:

1. Type-hints for return values in functions and methods receive syntax highlighting in the same way as type-hints for function and method parameters.

2. PHP Mode treats `yield from` as keyword in the same way it already does for a sole `yield`.

3. It recognizes `strict_types` as a special declaration in the same way as `ticks`.


Features
--------

### New Keywords

Now PHP Mode supports syntax highlighting for new keywords which PHP 5.4 introduced, e.g. those related to traits, such as `insteadof`.  Also supported are the older keywords `clone` and `default`.

### Constants

Syntax highlighting includes every magic constant and predefined constant listed on the official PHP site.  However, some constants from specific extensions are not currently included.

### Traits, Interfaces, and Namespaces

Traits, interfaces, and namespaces now appear under Imenu listings. Fontification behaves properly for namespaces as well, so that code like `namespace Foo\Bar\Baz` no longer looks like a warning.  This is also true for namespace aliases, e.g. `use <namespace> as <alias>`; currently the aliased name is not listed in Imenu, but future versions will address this.

### Treatment of Underscores

PHP Mode treats underscores as ‘symbol constituents’ (in Emacs terminology) so that you can use keys like `M-f` and `M-b` to move through the individual parts of a variable name like `$foo_bar_baz`.

### Chained Method Calls

PHP Mode can align method calls over multiple lines anchored around the `->` operator, e.g.:

```php
$object->foo()
       ->bar()
       ->baz();
```

This behaviour is off by default, but you can customize the variable `php-lineup-cascaded-calls` to enable this.

**Note:** Alignment will only work if you use one of the php-mode coding styles or inherit one of the styles.

### Nested Array Formatting

Nested function calls and `array()` structures now look better by default (or at least in my opinion).  Here is an example of the style:

```php
$results = Post::model()->find(
    array(
        'select' => 'title',
        'condition' => 'postID=:postID',
        'params' => array(':postID' => 10),
    )
);
```

### Anonymous Functions

Anonymous functions such as

```php
$greet = function($name) { ... };
```

will now appear on Imenu; in this case the name will be `$greet`.

### Flymake Support

By customizing the variable `php-executable` you can enable Flymake mode in order to see warnings and errors in real-time as you write code.

### Search Local Documentation

The key command `C-c C-f` will search the PHP website for documentation on the word under the cursor.  However, if you have a [local copy of the PHP documentation](http://us2.php.net/download-docs.php) then PHP Mode will try searching that documentation first.  All you need to do is customize the variable `php-manual-path` and give it the path to your copy of the documentation.  If PHP Mode cannot find something locally then it will still fallback on searching the PHP website.

### Executing Regions of PHP

The command `php-send-region`, which is bound to `C-c C-r` by default, will execute the selected region of PHP code.  In conjunction with the Emacs command `C-x h` you can use this to execute an entire file.  Any output will appear in a buffer called `*PHP*`.

### PHPDoc Tag / Annotation Highlighting

PHPDoc is a documentation format similar to [JavaDoc](https://en.wikipedia.org/wiki/Javadoc).

There are `@param`, `@return`, `@var`... etc in the notation called **tag**, look at [list of tags defined by phpDocumentor2](https://phpdoc.org/docs/latest/references/phpdoc/tags/index.html).  (These tags are compatible with static type checkers like PhpStorm and [Phan](https://github.com/etsy/phan).)

In addition, it also partially supports notation called **annotation**.  Annotation has a slightly different grammar from tag, and the example is `@Annotation(attr1="vvv", attr2="zzz")`.

[Symfony](http://symfony.com/) project and [Go! AOP](https://github.com/goaop/framework) and some projects/frameworks use annotation grammer based on [Doctrine Annotations](http://docs.doctrine-project.org/projects/doctrine-common/en/latest/reference/annotations.html).

```php
/**
 * Summary of Product class
 *
 * @copyright 2112 John Doe
 * @license https://spdx.org/licenses/Apache-2.0.html Apache License 2.0
 * @ORM\Entity
 * @ORM\Table(name="product")
 */
class Product
{
    /**
     * @ORM\Id
     * @ORM\Column(type="integer")
     * @ORM\GeneratedValue(strategy="AUTO")
     */
    protected $id;

    /**
     * @ORM\Column(type="string", length=100)
     */
    protected $name;

    /**
     * @ORM\Column(type="decimal", scale=2)
     */
    protected $price;

    /**
     * @ORM\Column(type="text")
     */
    protected $description;
}
```

The annotations are the lines that begin with the `@` character, and PHP Mode will give these special highlighting to help them stand out.

PHP Mode has not fully supported [PSR-5: PHPDoc (Draft)](https://github.com/phpDocumentor/fig-standards/blob/master/proposed/phpdoc.md) yet.  We want to support them, but the current implementation still limited.  See issue [#478](https://github.com/emacs-php/php-mode/issues/478) for details.

### Coding Styles

By default PHP Mode tries to provide a reasonable style for indentation and formatting, which you can use via the function `php-enable-default-coding-style`.  However, it provides other options suited for particular projects which you may find useful.  Other coding styles are available through these functions:

1. `php-enable-pear-coding-style`
2. `php-enable-drupal-coding-style`
3. `php-enable-wordpress-coding-style`
4. `php-enable-symfony2-coding-style`
5. `php-enable-psr2-coding-style`

They will help format your code for PEAR/PSR-2 projects, or work on Drupal, WordPress, and Symfony2 software, respectively.  You may enable any of them by default by running `M-x customize-group <RET> php` and looking for the ‘PHP Mode Coding Style’ option.  You may also enable any of these via a hook, e.g.

```lisp
(add-hook 'php-mode-hook 'php-enable-default-coding-style)
```

#### Symfony2 Style

With this style method call chains can be formatted with indented continuation and a hanging semi-colon:

```php
    $user1
        ->setCreateDate(new \DateTime('2007-05-07 01:34:45'))
        ->setLastDate(new \DateTime('2012-08-18 19:03:02'))
        ->setUsername('jay')
    ;
```

This style is used widely throughout Symfony2 source code even if it is not explicitly mentioned in their conventions documents.

### Extra Constants

If you commonly use a framework or library that defines a set of constants then you may wish to customize the value of `php-extra-constants`.  It is a list of strings that PHP Mode will treat as additional constants, i.e. providing them the same level syntax highlighting that PHP Mode uses for built-in constants.

### Web Mode Constants and Keywords

If you use [Web Mode][] then PHP Mode will attempt to use any additional PHP constants and keywords that Web Mode allows you to define.

### Avoid HTML Template Compatibility

Many developers use PHP Mode to edit pure PHP scripts (e.g. files with only PHP and no HTML). A basic compatibility layer with HTML has historically been part of PHP Mode but it does not work perfectly and can cause some bad side effects such as slowness and incorrect font locking.  Configuring the `php-template-compatibility` property with a `nil` will cancel any attempt of HTML compatibility.  [Web Mode](http://web-mode.org/) is a great alternative to PHP Mode if you need to work with PHP scripts that do contain HTML and other markup.

### Subword Mode

GNU Emacs comes with [Subword Mode][], a minor mode that allows you to navigate the parts of a [camelCase][] as if they were separate words.  For example, PHP Mode treats the variable `$fooBarBaz` as a whole name by default.  But if you enable Subword Mode then Emacs will treat the variable name as three separate words, and therefore word-related commands (e.g. `M-f`, `M-b`, `M-d`) will only affect the camelCase part of the name under the cursor.

If you want to always use Subword Mode for PHP files then you can add this to your Emacs configuration:

```lisp
(add-hook 'php-mode-hook (lambda () (subword-mode 1)))
```

The key-binding `C-c C-w` will also toggle Subword Mode on and off.

### Amaka Support

Viewing and editing build scripts for [Amaka](http://trashofmasters.github.io/amaka/) will automatically enable PHP Mode.

### Insert current class/namespace

```el
(with-eval-after-load 'php-mode
  (define-key php-mode-map (kbd "C-c C--") 'php-current-class)
  (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace))
```

Other Packages for PHP programming
----------------------------------

- Completions
  - [ac-php](https://github.com/xcwen/ac-php): [company-mode](https://github.com/company-mode/company-mode) and [auto-complete](https://github.com/auto-complete/auto-complete) for PHP
- Syntax checking
  - [flycheck](https://github.com/flycheck/flycheck/): On the fly syntax checker
  - [flymake-php](https://github.com/purcell/flymake-php): flymake for PHP files
- Snippet
  - [php-auto-yasnippets](https://github.com/ejmr/php-auto-yasnippets): Dynamically Generated YASnippets for PHP Code
- Documentation
  - [ggtags](https://github.com/leoliu/ggtags): eldoc by using GNU global tags
  - [php-eldoc](https://github.com/sabof/php-eldoc): eldoc backend for PHP
- Testing
  - [phpunit](https://github.com/nlamirault/phpunit.el): phpunit test command tool
- Style
  - [phpcbf](https://github.com/nishimaki10/emacs-phpcbf): PHP_CodeSniffer for Emacs
- Semantic
  - [ede-php-autoload](https://github.com/stevenremot/ede-php-autoload): Semantic for PHP
- Framework
  - [cake](https://github.com/k1LoW/emacs-cake): minor-mode for CakePHP
  - [cake2](https://github.com/k1LoW/emacs-cake2): minor-mode for CakePHP2


How to Contribute
-----------------

Please see [CONTRIBUTING.md](CONTRIBUTING.md#english).

The Wiki
--------

The GitHub project page has a [wiki][] that you should feel free to edit.  The wiki lists the features and bugs that are on plan to include in upcoming versions of PHP Mode.  It is also a place to add any tips to make the mode more useful.

License
-------

PHP Mode uses the [GNU General Public License 3](http://www.gnu.org/copyleft/gpl.html).


Contributors
------------

[![](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/images/0)](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/links/0)[![](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/images/1)](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/links/1)[![](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/images/2)](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/links/2)[![](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/images/3)](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/links/3)[![](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/images/4)](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/links/4)[![](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/images/5)](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/links/5)[![](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/images/6)](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/links/6)[![](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/images/7)](https://sourcerer.io/fame/ejmr/emacs-php/php-mode/links/7)

In chronological order:

1. Juanjo
2. Torsten Martinsen
3. Vinai Kopp
4. Sean Champ
5. Doug Marcey
6. Kevin Blake
7. Rex McMaster
8. Mathias Meyer
9. Boris Folgmann
10. Roland
11. Rosenfeld
12. Fred Yankowski
13. Craig Andrews
14. John Keller
15. Ryan
16. Sammartino
17. ppercot
18. Valentin Funk
19. Stig Bakken
20. Gregory Stark
21. Chris Morris
22. Nils Rennebarth
23. Gerrit Riessen
24. Eric Mc Sween
25. Ville Skytta
26. Giacomo Tesio
27. Urban Müller
28. [Engelke Eschner](https://github.com/tekai)
29. Lennart Borgman
30. Stefan Monnier
31. Aaron S. Hawley
32. [Ian Eure](https://github.com/ieure)
33. [Bill Lovett](https://github.com/lovett)
34. Dias Badekas
35. David House
36. [Tom Willemse](https://github.com/ryuslash)
37. [Olaf the Viking](https://github.com/olavTHEviking)
38. [Maël Nison](https://github.com/arcanis)
39. [flack](https://github.com/flack)
40. [Michele Bini](https://github.com/rev22)
41. Emanuele Tomasi
42. [David Maus](https://github.com/dmj)
43. [Jakub Jankiewicz](https://github.com/jcubic)
44. [Marcin Antczak](https://github.com/marcinant)
45. [顾伟刚](https://github.com/guweigang)
46. [zapad](https://github.com/zargener)
47. [Carl Groner](https://github.com/cgroner)
48. [Michael Dwyer](https://github.com/kalifg)
49. [Daniel Hackney](https://github.com/haxney)
50. [Nate Eagleson](https://github.com/NateEag)
51. [Steve Purcell](https://github.com/purcell)
52. TatriX
53. [François-Xavier Bois](https://github.com/fxbois)
54. [James Laver](https://github.com/jjl)
55. [Jacek Wysocki](https://github.com/exu)
56. [Jon Dufrense](https://github.com/jdufresne)
57. [Andrei Chițu](https://github.com/achitu)
58. [phil-s](https://github.com/phil-s)
59. [Bence Kalmar](https://github.com/brkalmar)
60. [Elis Axelsson](https://github.com/etu)
61. [Alan Pearce](https://github.com/alanpearce)
62. Syohei Yoshida
63. Joris Steyn
64. l3msh0
65. [Hernawan Fa'iz Abdillah](https://github.com/Abdillah)
66. [Sebastian Wiesner](https://github.com/lunaryorn)
67. [Michael Stolovitzsky](https://github.com/emestee)
68. [David Arroyo Menéndez](https://github.com/davidam)
69. [USAMI Kenta](https://tadsan.github.io/) (@zonuexe)
70. [Tim Landscheidt](http://www.tim-landscheidt.de)
71. [Fabian Wiget](https://github.com/fabacino)
72. tangxifan
73. [Serghei Iakovlev](https://github.com/sergeyklay)
74. [Christian Albrecht](https://github.com/calbrecht)
75. [Sebastian Fieber](https://github.com/fallchildren)
76. [Mark A. Hershberger](https://github.com/hexmode)


[Cask]: https://github.com/cask/cask
[MELPA Stable]: https://stable.melpa.org/
[MELPA]: https://melpa.org/
[Marmalade]: http://marmalade-repo.org/
[Subword Mode]: https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
[Supported Version]: https://github.com/emacs-php/php-mode/wiki/Supported-Version
[Web Mode]: http://web-mode.org/
[camelCase]: https://ja.wikipedia.org/wiki/%E3%82%AD%E3%83%A3%E3%83%A1%E3%83%AB%E3%82%B1%E3%83%BC%E3%82%B9
[cc mode]: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
[feathub]: https://feathub.com/emacs-php/php-suite
[melpa-badge]: http://melpa.org/packages/php-mode-badge.svg
[melpa-link]: http://melpa.org/#/php-mode
[melpa-stable-badge]: http://stable.melpa.org/packages/php-mode-badge.svg
[melpa-stable-link]: http://stable.melpa.org/#/php-mode
[package]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
[php-mode]: https://github.com/emacs-php/php-mode
[php-mode releases]: https://github.com/emacs-php/php-mode/releases
[php-suite]: https://github.com/emacs-php/php-suite
[travis-badge]: https://travis-ci.org/emacs-php/php-mode.svg
[travis-link]: https://travis-ci.org/emacs-php/php-mode
[wiki]: https://github.com/emacs-php/php-mode/wiki
