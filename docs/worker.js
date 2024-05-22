let wasm_bindgen = (function(exports) {
    let script_src;
    if (typeof document !== 'undefined' && document.currentScript !== null) {
        script_src = new URL(document.currentScript.src, location.href).toString();
    }

    class IntoUnderlyingByteSource {
        __destroy_into_raw() {
            const ptr = this.__wbg_ptr;
            this.__wbg_ptr = 0;
            IntoUnderlyingByteSourceFinalization.unregister(this);
            return ptr;
        }
        free() {
            const ptr = this.__destroy_into_raw();
            wasm.__wbg_intounderlyingbytesource_free(ptr, 0);
        }
        /**
         * @returns {number}
         */
        get autoAllocateChunkSize() {
            const ret = wasm.intounderlyingbytesource_autoAllocateChunkSize(this.__wbg_ptr);
            return ret >>> 0;
        }
        cancel() {
            const ptr = this.__destroy_into_raw();
            wasm.intounderlyingbytesource_cancel(ptr);
        }
        /**
         * @param {ReadableByteStreamController} controller
         * @returns {Promise<any>}
         */
        pull(controller) {
            const ret = wasm.intounderlyingbytesource_pull(this.__wbg_ptr, controller);
            return ret;
        }
        /**
         * @param {ReadableByteStreamController} controller
         */
        start(controller) {
            wasm.intounderlyingbytesource_start(this.__wbg_ptr, controller);
        }
        /**
         * @returns {ReadableStreamType}
         */
        get type() {
            const ret = wasm.intounderlyingbytesource_type(this.__wbg_ptr);
            return __wbindgen_enum_ReadableStreamType[ret];
        }
    }
    if (Symbol.dispose) IntoUnderlyingByteSource.prototype[Symbol.dispose] = IntoUnderlyingByteSource.prototype.free;

    class IntoUnderlyingSink {
        __destroy_into_raw() {
            const ptr = this.__wbg_ptr;
            this.__wbg_ptr = 0;
            IntoUnderlyingSinkFinalization.unregister(this);
            return ptr;
        }
        free() {
            const ptr = this.__destroy_into_raw();
            wasm.__wbg_intounderlyingsink_free(ptr, 0);
        }
        /**
         * @param {any} reason
         * @returns {Promise<any>}
         */
        abort(reason) {
            const ptr = this.__destroy_into_raw();
            const ret = wasm.intounderlyingsink_abort(ptr, reason);
            return ret;
        }
        /**
         * @returns {Promise<any>}
         */
        close() {
            const ptr = this.__destroy_into_raw();
            const ret = wasm.intounderlyingsink_close(ptr);
            return ret;
        }
        /**
         * @param {any} chunk
         * @returns {Promise<any>}
         */
        write(chunk) {
            const ret = wasm.intounderlyingsink_write(this.__wbg_ptr, chunk);
            return ret;
        }
    }
    if (Symbol.dispose) IntoUnderlyingSink.prototype[Symbol.dispose] = IntoUnderlyingSink.prototype.free;

    class IntoUnderlyingSource {
        __destroy_into_raw() {
            const ptr = this.__wbg_ptr;
            this.__wbg_ptr = 0;
            IntoUnderlyingSourceFinalization.unregister(this);
            return ptr;
        }
        free() {
            const ptr = this.__destroy_into_raw();
            wasm.__wbg_intounderlyingsource_free(ptr, 0);
        }
        cancel() {
            const ptr = this.__destroy_into_raw();
            wasm.intounderlyingsource_cancel(ptr);
        }
        /**
         * @param {ReadableStreamDefaultController} controller
         * @returns {Promise<any>}
         */
        pull(controller) {
            const ret = wasm.intounderlyingsource_pull(this.__wbg_ptr, controller);
            return ret;
        }
    }
    if (Symbol.dispose) IntoUnderlyingSource.prototype[Symbol.dispose] = IntoUnderlyingSource.prototype.free;
    function __wbg_get_imports() {
        const import0 = {
            __proto__: null,
            __wbg_Error_408e67f47ca7b58b: function(arg0, arg1) {
                const ret = Error(getStringFromWasm0(arg0, arg1));
                return ret;
            },
            __wbg_Number_3890faa6d3ff057d: function(arg0) {
                const ret = Number(arg0);
                return ret;
            },
            __wbg_String_8564e559799eccda: function(arg0, arg1) {
                const ret = String(arg1);
                const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
                const len1 = WASM_VECTOR_LEN;
                getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
                getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
            },
            __wbg___wbindgen_bigint_get_as_i64_c4ecf48528083721: function(arg0, arg1) {
                const v = arg1;
                const ret = typeof(v) === 'bigint' ? v : undefined;
                getDataViewMemory0().setBigInt64(arg0 + 8 * 1, isLikeNone(ret) ? BigInt(0) : ret, true);
                getDataViewMemory0().setInt32(arg0 + 4 * 0, !isLikeNone(ret), true);
            },
            __wbg___wbindgen_boolean_get_c9c83ebd41b34df3: function(arg0) {
                const v = arg0;
                const ret = typeof(v) === 'boolean' ? v : undefined;
                return isLikeNone(ret) ? 0xFFFFFF : ret ? 1 : 0;
            },
            __wbg___wbindgen_debug_string_a57024b9c6e4a48b: function(arg0, arg1) {
                const ret = debugString(arg1);
                const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
                const len1 = WASM_VECTOR_LEN;
                getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
                getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
            },
            __wbg___wbindgen_in_ac983077f137f2e6: function(arg0, arg1) {
                const ret = arg0 in arg1;
                return ret;
            },
            __wbg___wbindgen_is_bigint_8ffbbef442139384: function(arg0) {
                const ret = typeof(arg0) === 'bigint';
                return ret;
            },
            __wbg___wbindgen_is_function_5e4570eb24ffa122: function(arg0) {
                const ret = typeof(arg0) === 'function';
                return ret;
            },
            __wbg___wbindgen_is_object_a2790eb24c211ea0: function(arg0) {
                const val = arg0;
                const ret = typeof(val) === 'object' && val !== null;
                return ret;
            },
            __wbg___wbindgen_is_string_e6f02f0ea5f20a32: function(arg0) {
                const ret = typeof(arg0) === 'string';
                return ret;
            },
            __wbg___wbindgen_is_undefined_6cff064c44e0d823: function(arg0) {
                const ret = arg0 === undefined;
                return ret;
            },
            __wbg___wbindgen_jsval_eq_0a18949a61670320: function(arg0, arg1) {
                const ret = arg0 === arg1;
                return ret;
            },
            __wbg___wbindgen_jsval_loose_eq_acf2776254a8d832: function(arg0, arg1) {
                const ret = arg0 == arg1;
                return ret;
            },
            __wbg___wbindgen_number_get_136b9679cab35cfb: function(arg0, arg1) {
                const obj = arg1;
                const ret = typeof(obj) === 'number' ? obj : undefined;
                getDataViewMemory0().setFloat64(arg0 + 8 * 1, isLikeNone(ret) ? 0 : ret, true);
                getDataViewMemory0().setInt32(arg0 + 4 * 0, !isLikeNone(ret), true);
            },
            __wbg___wbindgen_string_get_d154f1e671052120: function(arg0, arg1) {
                const obj = arg1;
                const ret = typeof(obj) === 'string' ? obj : undefined;
                var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
                var len1 = WASM_VECTOR_LEN;
                getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
                getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
            },
            __wbg___wbindgen_throw_bb96b2010945f0bc: function(arg0, arg1) {
                throw new Error(getStringFromWasm0(arg0, arg1));
            },
            __wbg__wbg_cb_unref_be22cc64ae6946a0: function(arg0) {
                arg0._wbg_cb_unref();
            },
            __wbg_arrayBuffer_16433f17fbd74397: function() { return handleError(function (arg0) {
                const ret = arg0.arrayBuffer();
                return ret;
            }, arguments); },
            __wbg_buffer_78291c0e094ccf99: function(arg0) {
                const ret = arg0.buffer;
                return ret;
            },
            __wbg_buffer_8117fe4dab119813: function(arg0) {
                const ret = arg0.buffer;
                return ret;
            },
            __wbg_byobRequest_f8b1c89429b77545: function(arg0) {
                const ret = arg0.byobRequest;
                return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
            },
            __wbg_byteLength_336bc7d303511ba0: function(arg0) {
                const ret = arg0.byteLength;
                return ret;
            },
            __wbg_byteOffset_2b1d5b10453ce198: function(arg0) {
                const ret = arg0.byteOffset;
                return ret;
            },
            __wbg_call_1c5886ab9c57d1c7: function() { return handleError(function (arg0, arg1) {
                const ret = arg0.call(arg1);
                return ret;
            }, arguments); },
            __wbg_call_35dba3c747ad7521: function() { return handleError(function (arg0, arg1, arg2) {
                const ret = arg0.call(arg1, arg2);
                return ret;
            }, arguments); },
            __wbg_clipboard_4fea7f044e5b8637: function(arg0) {
                const ret = arg0.clipboard;
                return ret;
            },
            __wbg_close_72f69f5f2de2bc73: function() { return handleError(function (arg0) {
                arg0.close();
            }, arguments); },
            __wbg_close_97cdb44c3a7878f6: function() { return handleError(function (arg0) {
                arg0.close();
            }, arguments); },
            __wbg_data_57d8ce4eb5f0a433: function(arg0) {
                const ret = arg0.data;
                return ret;
            },
            __wbg_done_669171204c3dcae2: function(arg0) {
                const ret = arg0.done;
                return ret;
            },
            __wbg_enqueue_7d68a21eda78e72f: function() { return handleError(function (arg0, arg1) {
                arg0.enqueue(arg1);
            }, arguments); },
            __wbg_entries_7774d489e1da5f4f: function(arg0) {
                const ret = Object.entries(arg0);
                return ret;
            },
            __wbg_error_757e9472f8410341: function(arg0, arg1) {
                let deferred0_0;
                let deferred0_1;
                try {
                    deferred0_0 = arg0;
                    deferred0_1 = arg1;
                    console.error(getStringFromWasm0(arg0, arg1));
                } finally {
                    wasm.__wbindgen_free(deferred0_0, deferred0_1, 1);
                }
            },
            __wbg_fetch_729fad2e5272298f: function(arg0, arg1) {
                const ret = arg0.fetch(arg1);
                return ret;
            },
            __wbg_fetch_d752d93f5b259503: function(arg0, arg1) {
                const ret = arg0.fetch(arg1);
                return ret;
            },
            __wbg_from_74f3d90e0ff11240: function(arg0) {
                const ret = Array.from(arg0);
                return ret;
            },
            __wbg_getItem_eb388fb8c39edb35: function() { return handleError(function (arg0, arg1, arg2, arg3) {
                const ret = arg1.getItem(getStringFromWasm0(arg2, arg3));
                var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
                var len1 = WASM_VECTOR_LEN;
                getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
                getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
            }, arguments); },
            __wbg_getRandomValues_7047d8fa53b34af5: function() { return handleError(function (arg0, arg1) {
                globalThis.crypto.getRandomValues(getArrayU8FromWasm0(arg0, arg1));
            }, arguments); },
            __wbg_getTimezoneOffset_4baa793e0d3962a8: function(arg0) {
                const ret = arg0.getTimezoneOffset();
                return ret;
            },
            __wbg_get_c0c8f8d7da0c03dd: function(arg0, arg1) {
                const ret = arg0[arg1 >>> 0];
                return ret;
            },
            __wbg_get_d173c0308df22d37: function() { return handleError(function (arg0, arg1) {
                const ret = Reflect.get(arg0, arg1);
                return ret;
            }, arguments); },
            __wbg_get_unchecked_e20b893aeafc3fca: function(arg0, arg1) {
                const ret = arg0[arg1 >>> 0];
                return ret;
            },
            __wbg_get_with_ref_key_6412cf3094599694: function(arg0, arg1) {
                const ret = arg0[arg1];
                return ret;
            },
            __wbg_instanceof_ArrayBuffer_993d02d2d254cad1: function(arg0) {
                let result;
                try {
                    result = arg0 instanceof ArrayBuffer;
                } catch (_) {
                    result = false;
                }
                const ret = result;
                return ret;
            },
            __wbg_instanceof_Response_8f49efbd4bfd76d6: function(arg0) {
                let result;
                try {
                    result = arg0 instanceof Response;
                } catch (_) {
                    result = false;
                }
                const ret = result;
                return ret;
            },
            __wbg_instanceof_Uint8Array_f935dbb0aa7cdeed: function(arg0) {
                let result;
                try {
                    result = arg0 instanceof Uint8Array;
                } catch (_) {
                    result = false;
                }
                const ret = result;
                return ret;
            },
            __wbg_instanceof_Window_5625ff9937037a38: function(arg0) {
                let result;
                try {
                    result = arg0 instanceof Window;
                } catch (_) {
                    result = false;
                }
                const ret = result;
                return ret;
            },
            __wbg_isArray_6339f732981044bf: function(arg0) {
                const ret = Array.isArray(arg0);
                return ret;
            },
            __wbg_isSafeInteger_f3d6cd19ccfe4512: function(arg0) {
                const ret = Number.isSafeInteger(arg0);
                return ret;
            },
            __wbg_iterator_5cebbb86e33c6dd6: function() {
                const ret = Symbol.iterator;
                return ret;
            },
            __wbg_length_36bd29c6848c2144: function(arg0) {
                const ret = arg0.length;
                return ret;
            },
            __wbg_length_ecfa2c63d3d0d82c: function(arg0) {
                const ret = arg0.length;
                return ret;
            },
            __wbg_localStorage_19bddab1e4cb2413: function() { return handleError(function (arg0) {
                const ret = arg0.localStorage;
                return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
            }, arguments); },
            __wbg_log_e6372b4fbfc9f81e: function(arg0) {
                console.log(arg0);
            },
            __wbg_navigator_6cfdd5fa246d910f: function(arg0) {
                const ret = arg0.navigator;
                return ret;
            },
            __wbg_new_0_f117d868b403dc07: function() {
                const ret = new Date();
                return ret;
            },
            __wbg_new_116be93542d39019: function() {
                const ret = new Array();
                return ret;
            },
            __wbg_new_227d7c05414eb861: function() {
                const ret = new Error();
                return ret;
            },
            __wbg_new_358857d90afd5a2d: function(arg0, arg1) {
                const ret = new Error(getStringFromWasm0(arg0, arg1));
                return ret;
            },
            __wbg_new_77cc4f4f472aeb81: function(arg0) {
                const ret = new Uint8Array(arg0);
                return ret;
            },
            __wbg_new_cce9e3b360a5a9ac: function() { return handleError(function () {
                const ret = new XMLHttpRequest();
                return ret;
            }, arguments); },
            __wbg_new_ebe3e0f6837f0879: function() {
                const ret = new Object();
                return ret;
            },
            __wbg_new_f545e2e1ea609f29: function(arg0) {
                const ret = new Int32Array(arg0);
                return ret;
            },
            __wbg_new_fee1181bcca662c4: function() { return handleError(function (arg0) {
                const ret = new WebAssembly.Memory(arg0);
                return ret;
            }, arguments); },
            __wbg_new_typed_cceaf62d8d95e9f2: function(arg0, arg1) {
                try {
                    var state0 = {a: arg0, b: arg1};
                    var cb0 = (arg0, arg1) => {
                        const a = state0.a;
                        state0.a = 0;
                        try {
                            return wasm_bindgen_d646e8569da5407e___convert__closures_____invoke___js_sys_45c940744d82c88b___Function_fn_wasm_bindgen_d646e8569da5407e___JsValue_____wasm_bindgen_d646e8569da5407e___sys__Undefined___js_sys_45c940744d82c88b___Function_fn_wasm_bindgen_d646e8569da5407e___JsValue_____wasm_bindgen_d646e8569da5407e___sys__Undefined_______true_(a, state0.b, arg0, arg1);
                        } finally {
                            state0.a = a;
                        }
                    };
                    const ret = new Promise(cb0);
                    return ret;
                } finally {
                    state0.a = 0;
                }
            },
            __wbg_new_with_byte_offset_and_length_ff6e927f8d72f0c3: function(arg0, arg1, arg2) {
                const ret = new Uint8Array(arg0, arg1 >>> 0, arg2 >>> 0);
                return ret;
            },
            __wbg_new_with_src_927e740ff690775d: function() { return handleError(function (arg0, arg1) {
                const ret = new Audio(getStringFromWasm0(arg0, arg1));
                return ret;
            }, arguments); },
            __wbg_new_with_str_and_init_5a37d576dec75a86: function() { return handleError(function (arg0, arg1, arg2) {
                const ret = new Request(getStringFromWasm0(arg0, arg1), arg2);
                return ret;
            }, arguments); },
            __wbg_next_42cf16ee0dafc9e2: function() { return handleError(function (arg0) {
                const ret = arg0.next();
                return ret;
            }, arguments); },
            __wbg_next_8f26b64fa5e9f64b: function(arg0) {
                const ret = arg0.next;
                return ret;
            },
            __wbg_now_23a6155d40a5db5a: function() {
                const ret = performance.now();
                return ret;
            },
            __wbg_now_8b265300afd5f2b9: function() {
                const ret = Date.now();
                return ret;
            },
            __wbg_open_e16d955273173e11: function() { return handleError(function (arg0, arg1, arg2, arg3, arg4, arg5) {
                arg0.open(getStringFromWasm0(arg1, arg2), getStringFromWasm0(arg3, arg4), arg5 !== 0);
            }, arguments); },
            __wbg_play_c3e69b97ecaf6d8e: function() { return handleError(function (arg0) {
                const ret = arg0.play();
                return ret;
            }, arguments); },
            __wbg_postMessage_e86c0107e98b92c3: function() { return handleError(function (arg0, arg1, arg2) {
                arg0.postMessage(arg1, arg2);
            }, arguments); },
            __wbg_prompt_af4388f84af176b4: function() { return handleError(function (arg0, arg1, arg2, arg3) {
                const ret = arg1.prompt(getStringFromWasm0(arg2, arg3));
                var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
                var len1 = WASM_VECTOR_LEN;
                getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
                getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
            }, arguments); },
            __wbg_prototypesetcall_de8e0d9553586985: function(arg0, arg1, arg2) {
                Uint8Array.prototype.set.call(getArrayU8FromWasm0(arg0, arg1), arg2);
            },
            __wbg_push_adb0107829f02d75: function(arg0, arg1) {
                const ret = arg0.push(arg1);
                return ret;
            },
            __wbg_queueMicrotask_ac694eae12e92dfb: function(arg0) {
                queueMicrotask(arg0);
            },
            __wbg_queueMicrotask_be5fe34a8f4cad4d: function(arg0) {
                const ret = arg0.queueMicrotask;
                return ret;
            },
            __wbg_resolve_020f95d838c6ef25: function(arg0) {
                const ret = Promise.resolve(arg0);
                return ret;
            },
            __wbg_respond_f88cbcebace42068: function() { return handleError(function (arg0, arg1) {
                arg0.respond(arg1 >>> 0);
            }, arguments); },
            __wbg_response_10a39d3835b35271: function() { return handleError(function (arg0) {
                const ret = arg0.response;
                return ret;
            }, arguments); },
            __wbg_send_adfe0f3b2360a40b: function() { return handleError(function (arg0) {
                arg0.send();
            }, arguments); },
            __wbg_set_6be42768c690e380: function(arg0, arg1, arg2) {
                arg0[arg1] = arg2;
            },
            __wbg_set_8155bb79a948541b: function() { return handleError(function (arg0, arg1, arg2) {
                const ret = Reflect.set(arg0, arg1, arg2);
                return ret;
            }, arguments); },
            __wbg_set_a80955eb93b145c6: function(arg0, arg1, arg2) {
                arg0[arg1 >>> 0] = arg2;
            },
            __wbg_set_b9b5b5cb7b495037: function(arg0, arg1, arg2) {
                arg0.set(getArrayU8FromWasm0(arg1, arg2));
            },
            __wbg_set_method_cf2b992b9a610bc3: function(arg0, arg1, arg2) {
                arg0.method = getStringFromWasm0(arg1, arg2);
            },
            __wbg_set_mode_d6479dfd6696c8d3: function(arg0, arg1) {
                arg0.mode = __wbindgen_enum_RequestMode[arg1];
            },
            __wbg_set_onmessage_e3cf4ebd488be189: function(arg0, arg1) {
                arg0.onmessage = arg1;
            },
            __wbg_set_responseType_1776b04deaa63a64: function(arg0, arg1) {
                arg0.responseType = __wbindgen_enum_XmlHttpRequestResponseType[arg1];
            },
            __wbg_stack_3b0d974bbf31e44f: function(arg0, arg1) {
                const ret = arg1.stack;
                const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
                const len1 = WASM_VECTOR_LEN;
                getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
                getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
            },
            __wbg_static_accessor_GLOBAL_THIS_466428f93b4eaa76: function() {
                const ret = typeof globalThis === 'undefined' ? null : globalThis;
                return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
            },
            __wbg_static_accessor_GLOBAL_c7aea38d4de089bc: function() {
                const ret = typeof global === 'undefined' ? null : global;
                return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
            },
            __wbg_static_accessor_SELF_42d4fae05e59267a: function() {
                const ret = typeof self === 'undefined' ? null : self;
                return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
            },
            __wbg_static_accessor_WINDOW_e0db14a0eba6a812: function() {
                const ret = typeof window === 'undefined' ? null : window;
                return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
            },
            __wbg_status_89c2902a3cf3e574: function() { return handleError(function (arg0) {
                const ret = arg0.status;
                return ret;
            }, arguments); },
            __wbg_status_b0de02a07fd7d927: function(arg0) {
                const ret = arg0.status;
                return ret;
            },
            __wbg_text_9302f33ea8cfce7b: function() { return handleError(function (arg0) {
                const ret = arg0.text();
                return ret;
            }, arguments); },
            __wbg_then_7026b513a94278a8: function(arg0, arg1) {
                const ret = arg0.then(arg1);
                return ret;
            },
            __wbg_then_72819b8d4e081fb5: function(arg0, arg1, arg2) {
                const ret = arg0.then(arg1, arg2);
                return ret;
            },
            __wbg_value_1e2369fab29b420e: function(arg0) {
                const ret = arg0.value;
                return ret;
            },
            __wbg_values_bbaf1bcbcd5d1deb: function(arg0) {
                const ret = Object.values(arg0);
                return ret;
            },
            __wbg_view_7685fe4b2845c5b6: function(arg0) {
                const ret = arg0.view;
                return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
            },
            __wbg_wait_5f03a1b29e76c8cb: function() { return handleError(function (arg0, arg1, arg2, arg3) {
                const ret = Atomics.wait(arg0, arg1 >>> 0, arg2, arg3);
                return ret;
            }, arguments); },
            __wbg_writeText_0346496782732979: function(arg0, arg1, arg2) {
                const ret = arg0.writeText(getStringFromWasm0(arg1, arg2));
                return ret;
            },
            __wbindgen_cast_0000000000000001: function(arg0, arg1) {
                // Cast intrinsic for `Closure(Closure { owned: true, function: Function { arguments: [Externref], shim_idx: 242, ret: Result(Unit), inner_ret: Some(Result(Unit)) }, mutable: true }) -> Externref`.
                const ret = makeMutClosure(arg0, arg1, wasm_bindgen_d646e8569da5407e___convert__closures_____invoke___wasm_bindgen_d646e8569da5407e___JsValue__core_d941ad8722425d2e___result__Result_____wasm_bindgen_d646e8569da5407e___JsError___true_);
                return ret;
            },
            __wbindgen_cast_0000000000000002: function(arg0, arg1) {
                // Cast intrinsic for `Closure(Closure { owned: true, function: Function { arguments: [NamedExternref("MessageEvent")], shim_idx: 1, ret: Unit, inner_ret: Some(Unit) }, mutable: true }) -> Externref`.
                const ret = makeMutClosure(arg0, arg1, wasm_bindgen_d646e8569da5407e___convert__closures_____invoke___web_sys_5bf266987872c18c___features__gen_MessageEvent__MessageEvent______true_);
                return ret;
            },
            __wbindgen_cast_0000000000000003: function(arg0) {
                // Cast intrinsic for `F64 -> Externref`.
                const ret = arg0;
                return ret;
            },
            __wbindgen_cast_0000000000000004: function(arg0, arg1) {
                // Cast intrinsic for `Ref(Slice(U8)) -> NamedExternref("Uint8Array")`.
                const ret = getArrayU8FromWasm0(arg0, arg1);
                return ret;
            },
            __wbindgen_cast_0000000000000005: function(arg0, arg1) {
                // Cast intrinsic for `Ref(String) -> Externref`.
                const ret = getStringFromWasm0(arg0, arg1);
                return ret;
            },
            __wbindgen_cast_0000000000000006: function(arg0) {
                // Cast intrinsic for `U64 -> Externref`.
                const ret = BigInt.asUintN(64, arg0);
                return ret;
            },
            __wbindgen_init_externref_table: function() {
                const table = wasm.__wbindgen_externrefs;
                const offset = table.grow(4);
                table.set(0, undefined);
                table.set(offset + 0, undefined);
                table.set(offset + 1, null);
                table.set(offset + 2, true);
                table.set(offset + 3, false);
            },
        };
        return {
            __proto__: null,
            "./worker_bg.js": import0,
        };
    }

    function wasm_bindgen_d646e8569da5407e___convert__closures_____invoke___web_sys_5bf266987872c18c___features__gen_MessageEvent__MessageEvent______true_(arg0, arg1, arg2) {
        wasm.wasm_bindgen_d646e8569da5407e___convert__closures_____invoke___web_sys_5bf266987872c18c___features__gen_MessageEvent__MessageEvent______true_(arg0, arg1, arg2);
    }

    function wasm_bindgen_d646e8569da5407e___convert__closures_____invoke___wasm_bindgen_d646e8569da5407e___JsValue__core_d941ad8722425d2e___result__Result_____wasm_bindgen_d646e8569da5407e___JsError___true_(arg0, arg1, arg2) {
        const ret = wasm.wasm_bindgen_d646e8569da5407e___convert__closures_____invoke___wasm_bindgen_d646e8569da5407e___JsValue__core_d941ad8722425d2e___result__Result_____wasm_bindgen_d646e8569da5407e___JsError___true_(arg0, arg1, arg2);
        if (ret[1]) {
            throw takeFromExternrefTable0(ret[0]);
        }
    }

    function wasm_bindgen_d646e8569da5407e___convert__closures_____invoke___js_sys_45c940744d82c88b___Function_fn_wasm_bindgen_d646e8569da5407e___JsValue_____wasm_bindgen_d646e8569da5407e___sys__Undefined___js_sys_45c940744d82c88b___Function_fn_wasm_bindgen_d646e8569da5407e___JsValue_____wasm_bindgen_d646e8569da5407e___sys__Undefined_______true_(arg0, arg1, arg2, arg3) {
        wasm.wasm_bindgen_d646e8569da5407e___convert__closures_____invoke___js_sys_45c940744d82c88b___Function_fn_wasm_bindgen_d646e8569da5407e___JsValue_____wasm_bindgen_d646e8569da5407e___sys__Undefined___js_sys_45c940744d82c88b___Function_fn_wasm_bindgen_d646e8569da5407e___JsValue_____wasm_bindgen_d646e8569da5407e___sys__Undefined_______true_(arg0, arg1, arg2, arg3);
    }


    const __wbindgen_enum_ReadableStreamType = ["bytes"];


    const __wbindgen_enum_RequestMode = ["same-origin", "no-cors", "cors", "navigate"];


    const __wbindgen_enum_XmlHttpRequestResponseType = ["", "arraybuffer", "blob", "document", "json", "text"];
    const IntoUnderlyingByteSourceFinalization = (typeof FinalizationRegistry === 'undefined')
        ? { register: () => {}, unregister: () => {} }
        : new FinalizationRegistry(ptr => wasm.__wbg_intounderlyingbytesource_free(ptr, 1));
    const IntoUnderlyingSinkFinalization = (typeof FinalizationRegistry === 'undefined')
        ? { register: () => {}, unregister: () => {} }
        : new FinalizationRegistry(ptr => wasm.__wbg_intounderlyingsink_free(ptr, 1));
    const IntoUnderlyingSourceFinalization = (typeof FinalizationRegistry === 'undefined')
        ? { register: () => {}, unregister: () => {} }
        : new FinalizationRegistry(ptr => wasm.__wbg_intounderlyingsource_free(ptr, 1));

    function addToExternrefTable0(obj) {
        const idx = wasm.__externref_table_alloc();
        wasm.__wbindgen_externrefs.set(idx, obj);
        return idx;
    }

    const CLOSURE_DTORS = (typeof FinalizationRegistry === 'undefined')
        ? { register: () => {}, unregister: () => {} }
        : new FinalizationRegistry(state => wasm.__wbindgen_destroy_closure(state.a, state.b));

    function debugString(val) {
        // primitive types
        const type = typeof val;
        if (type == 'number' || type == 'boolean' || val == null) {
            return  `${val}`;
        }
        if (type == 'string') {
            return `"${val}"`;
        }
        if (type == 'symbol') {
            const description = val.description;
            if (description == null) {
                return 'Symbol';
            } else {
                return `Symbol(${description})`;
            }
        }
        if (type == 'function') {
            const name = val.name;
            if (typeof name == 'string' && name.length > 0) {
                return `Function(${name})`;
            } else {
                return 'Function';
            }
        }
        // objects
        if (Array.isArray(val)) {
            const length = val.length;
            let debug = '[';
            if (length > 0) {
                debug += debugString(val[0]);
            }
            for(let i = 1; i < length; i++) {
                debug += ', ' + debugString(val[i]);
            }
            debug += ']';
            return debug;
        }
        // Test for built-in
        const builtInMatches = /\[object ([^\]]+)\]/.exec(toString.call(val));
        let className;
        if (builtInMatches && builtInMatches.length > 1) {
            className = builtInMatches[1];
        } else {
            // Failed to match the standard '[object ClassName]'
            return toString.call(val);
        }
        if (className == 'Object') {
            // we're a user defined class or Object
            // JSON.stringify avoids problems with cycles, and is generally much
            // easier than looping through ownProperties of `val`.
            try {
                return 'Object(' + JSON.stringify(val) + ')';
            } catch (_) {
                return 'Object';
            }
        }
        // errors
        if (val instanceof Error) {
            return `${val.name}: ${val.message}\n${val.stack}`;
        }
        // TODO we could test for more things here, like `Set`s and `Map`s.
        return className;
    }

    function getArrayU8FromWasm0(ptr, len) {
        ptr = ptr >>> 0;
        return getUint8ArrayMemory0().subarray(ptr / 1, ptr / 1 + len);
    }

    let cachedDataViewMemory0 = null;
    function getDataViewMemory0() {
        if (cachedDataViewMemory0 === null || cachedDataViewMemory0.buffer.detached === true || (cachedDataViewMemory0.buffer.detached === undefined && cachedDataViewMemory0.buffer !== wasm.memory.buffer)) {
            cachedDataViewMemory0 = new DataView(wasm.memory.buffer);
        }
        return cachedDataViewMemory0;
    }

    function getStringFromWasm0(ptr, len) {
        return decodeText(ptr >>> 0, len);
    }

    let cachedUint8ArrayMemory0 = null;
    function getUint8ArrayMemory0() {
        if (cachedUint8ArrayMemory0 === null || cachedUint8ArrayMemory0.byteLength === 0) {
            cachedUint8ArrayMemory0 = new Uint8Array(wasm.memory.buffer);
        }
        return cachedUint8ArrayMemory0;
    }

    function handleError(f, args) {
        try {
            return f.apply(this, args);
        } catch (e) {
            const idx = addToExternrefTable0(e);
            wasm.__wbindgen_exn_store(idx);
        }
    }

    function isLikeNone(x) {
        return x === undefined || x === null;
    }

    function makeMutClosure(arg0, arg1, f) {
        const state = { a: arg0, b: arg1, cnt: 1 };
        const real = (...args) => {

            // First up with a closure we increment the internal reference
            // count. This ensures that the Rust closure environment won't
            // be deallocated while we're invoking it.
            state.cnt++;
            const a = state.a;
            state.a = 0;
            try {
                return f(a, state.b, ...args);
            } finally {
                state.a = a;
                real._wbg_cb_unref();
            }
        };
        real._wbg_cb_unref = () => {
            if (--state.cnt === 0) {
                wasm.__wbindgen_destroy_closure(state.a, state.b);
                state.a = 0;
                CLOSURE_DTORS.unregister(state);
            }
        };
        CLOSURE_DTORS.register(real, state, state);
        return real;
    }

    function passStringToWasm0(arg, malloc, realloc) {
        if (realloc === undefined) {
            const buf = cachedTextEncoder.encode(arg);
            const ptr = malloc(buf.length, 1) >>> 0;
            getUint8ArrayMemory0().subarray(ptr, ptr + buf.length).set(buf);
            WASM_VECTOR_LEN = buf.length;
            return ptr;
        }

        let len = arg.length;
        let ptr = malloc(len, 1) >>> 0;

        const mem = getUint8ArrayMemory0();

        let offset = 0;

        for (; offset < len; offset++) {
            const code = arg.charCodeAt(offset);
            if (code > 0x7F) break;
            mem[ptr + offset] = code;
        }
        if (offset !== len) {
            if (offset !== 0) {
                arg = arg.slice(offset);
            }
            ptr = realloc(ptr, len, len = offset + arg.length * 3, 1) >>> 0;
            const view = getUint8ArrayMemory0().subarray(ptr + offset, ptr + len);
            const ret = cachedTextEncoder.encodeInto(arg, view);

            offset += ret.written;
            ptr = realloc(ptr, len, offset, 1) >>> 0;
        }

        WASM_VECTOR_LEN = offset;
        return ptr;
    }

    function takeFromExternrefTable0(idx) {
        const value = wasm.__wbindgen_externrefs.get(idx);
        wasm.__externref_table_dealloc(idx);
        return value;
    }

    let cachedTextDecoder = new TextDecoder('utf-8', { ignoreBOM: true, fatal: true });
    cachedTextDecoder.decode();
    function decodeText(ptr, len) {
        return cachedTextDecoder.decode(getUint8ArrayMemory0().subarray(ptr, ptr + len));
    }

    const cachedTextEncoder = new TextEncoder();

    if (!('encodeInto' in cachedTextEncoder)) {
        cachedTextEncoder.encodeInto = function (arg, view) {
            const buf = cachedTextEncoder.encode(arg);
            view.set(buf);
            return {
                read: arg.length,
                written: buf.length
            };
        };
    }

    let WASM_VECTOR_LEN = 0;

    let wasmModule, wasmInstance, wasm;
    function __wbg_finalize_init(instance, module) {
        wasmInstance = instance;
        wasm = instance.exports;
        wasmModule = module;
        cachedDataViewMemory0 = null;
        cachedUint8ArrayMemory0 = null;
        wasm.__wbindgen_start();
        return wasm;
    }

    async function __wbg_load(module, imports) {
        if (typeof Response === 'function' && module instanceof Response) {
            if (!module.ok) {
                throw new Error(`failed to fetch Wasm: ${module.status} ${module.statusText} fetching '${module.url}'`);
            }

            if (typeof WebAssembly.instantiateStreaming === 'function') {
                try {
                    return await WebAssembly.instantiateStreaming(module, imports);
                } catch (e) {
                    const validResponse = expectedResponseType(module.type);

                    if (validResponse && module.headers.get('Content-Type') !== 'application/wasm') {
                        console.warn("`WebAssembly.instantiateStreaming` failed because your server does not serve Wasm with `application/wasm` MIME type. Falling back to `WebAssembly.instantiate` which is slower. Original error:\n", e);

                    } else { throw e; }
                }
            }

            const bytes = await module.arrayBuffer();
            return await WebAssembly.instantiate(bytes, imports);
        } else {
            const instance = await WebAssembly.instantiate(module, imports);

            if (instance instanceof WebAssembly.Instance) {
                return { instance, module };
            } else {
                return instance;
            }
        }

        function expectedResponseType(type) {
            switch (type) {
                case 'basic': case 'cors': case 'default': return true;
            }
            return false;
        }
    }

    function initSync(module) {
        if (wasm !== undefined) return wasm;


        if (module !== undefined) {
            if (Object.getPrototypeOf(module) === Object.prototype) {
                ({module} = module)
            } else {
                console.warn('using deprecated parameters for `initSync()`; pass a single object instead')
            }
        }

        const imports = __wbg_get_imports();
        if (!(module instanceof WebAssembly.Module)) {
            module = new WebAssembly.Module(module);
        }
        const instance = new WebAssembly.Instance(module, imports);
        return __wbg_finalize_init(instance, module);
    }

    async function __wbg_init(module_or_path) {
        if (wasm !== undefined) return wasm;


        if (module_or_path !== undefined) {
            if (Object.getPrototypeOf(module_or_path) === Object.prototype) {
                ({module_or_path} = module_or_path)
            } else {
                console.warn('using deprecated parameters for the initialization function; pass a single object instead')
            }
        }

        if (module_or_path === undefined && script_src !== undefined) {
            module_or_path = script_src.replace(/\.js$/, "_bg.wasm");
        }
        const imports = __wbg_get_imports();

        if (typeof module_or_path === 'string' || (typeof Request === 'function' && module_or_path instanceof Request) || (typeof URL === 'function' && module_or_path instanceof URL)) {
            module_or_path = fetch(module_or_path);
        }

        const { instance, module } = await __wbg_load(await module_or_path, imports);

        return __wbg_finalize_init(instance, module);
    }

    return Object.assign(__wbg_init, { initSync }, exports);
})({ __proto__: null });
