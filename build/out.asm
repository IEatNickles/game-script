global main
section .text
main:
	push rbp
	mov rbp, rsp
;; Begin

;; var 'x'
	mov rax, 0
	push rax


;; ref 'x'
	push QWORD [rsp + 0]

	pop rax
	test rax, rax
	jz .label1
;; assign 'x'
	mov rax, 1
	push rax

	pop rax
	mov QWORD [rsp + 0], rax


	jmp .label2
.label1:
	mov rax, 2
	push rax

;; ref 'x'
	push QWORD [rsp + 8]

	pop rax
	pop rbx
	add rax, rbx
	push rax

	pop rax
	test rax, rax
	jz .label3
;; assign 'x'
	mov rax, 2
	push rax

	pop rax
	mov QWORD [rsp + 0], rax


.label3:
.label2:

;; ref 'x'
	push QWORD [rsp + 0]

	pop rax
	leave
	ret

