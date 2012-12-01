;;;;
;;;; bindings.scm
;;;;
;;;; Bindings used to interface with c code.
;;;;

(##include "ffi.sch")
(c-include "gfx.h" 'local)
(c-include "GL/glfw.h")

(c-define-type GLuint "GLuint")
(c-define-type GLenum "GLenum")

(generate-release-fn "texture_delete")
(c-define-type texture* 
               (pointer "texture_t" (texture_t)))

(define-c-struct rect "rect_t"
  (int x)
  (int y)
  (int width)
  (int height))

(define-c-struct color "color_t"
  (unsigned-int8 red)
  (unsigned-int8 green)
  (unsigned-int8 blue)
  (unsigned-int8 alpha))

(define (load-texture filename)
  (define true-lambda
    (c-lambda (nonnull-char-string bool) texture* "texture_load"))
  (true-lambda filename #t))

(define gfx-blit
  (c-lambda (texture* int int)
            void
#<<C-LAMBDA-END
  color_t color = { 255, 255, 255, 255 };
  rect_t  src_area = { 0, 0, ___arg1->width, ___arg1->height };
  rect_t  dest_area = { ___arg2, ___arg3, 0, 0 };
  
  gfx_blit(___arg1, &src_area, &dest_area, &color);
C-LAMBDA-END
))

;;;
;;; sprite sheets
;;;
(generate-release-fn "sprite_sheet_delete")
(c-define-type sprite-sheet*
               (pointer "sprite_sheet_t" (sprite-sheet-t) 
                        "sprite_sheet_delete_release"))

(define make-sprite-sheet
  (c-lambda (texture* int int)
            sprite-sheet*
            "sprite_sheet_new"))

(c-define-type sprite* 
               (pointer "sprite_t" (sprite-t)))

(define get-sprite
  (c-lambda (sprite-sheet* int int)
            sprite*
            "sprite_sheet_get_sprite"))

(define (render-sprite spr x y #!key (color '(255 255 255 255)))
  (define inner-lambda
    (c-lambda (sprite* int int int int int int)
              void
#<<C-LAMBDA-END
  color_t color = { ___arg4, ___arg5, ___arg6, ___arg7 };
  rect_t  dest_area = { ___arg2, ___arg3, 0, 0 };

  sprite_render(___arg1, &dest_area, &color);
C-LAMBDA-END
))
  (inner-lambda spr x y 
                (car color)
                (cadr color)
                (caddr color)
                (cadddr color)))

(define (sprite/get-width sprite)
  (c-lambda (sprite*) int "___result = ___arg1->area.width;"))

(define (sprite/get-height sprite)
  (c-lambda (sprite*) int "___result = ___arg1->area.height;"))

;;;
;;; fonts
;;;

(generate-release-fn "font_delete")
(c-define-type font*
               (pointer "font_t" (font-t) 
                        "font_delete_release"))

(define make-font
  (c-lambda (texture* int int)
            font*
            "font_new"))

(define (render-string font x y str #!key (color '(255 255 255 255)))
  (define inner-lambda
    (c-lambda (font* int int nonnull-char-string int int int int)
              void
#<<C-LAMBDA-END
  color_t color = { ___arg5, ___arg6, ___arg7, ___arg8 };
  font_render_string(___arg1, ___arg2, ___arg3, ___arg4, &color);
C-LAMBDA-END
))
  (inner-lambda font x y str
                (car color)
                (cadr color)
                (caddr color)
                (cadddr color)))

;;;
;;; general graphics
;;;

(define gfx/get-width (c-lambda () int "gfx_get_width"))
(define gfx/get-height (c-lambda () int "gfx_get_height"))

;;;
;;; input bindings
;;;

(define key-released 0)
(define key-pressed 1)

(define-enum key
  (special 256)
  esc
  f1
  f2
  f3
  f4
  f5
  f6
  f7
  f8
  f9
  f10
  f11
  f12
  f13
  f14
  f15
  f16
  f17
  f18
  f19
  f20
  f21
  f22
  f23
  f24
  f25
  up
  down
  left
  right
  lshift
  rshift
  lctrl
  rctrl
  lalt
  ralt
  tab
  enter
  backspace
  insert
  del)

(define glfw/get-key
  (c-lambda (int) int "___result = glfwGetKey(___arg1);"))
