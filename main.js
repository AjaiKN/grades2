!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function a(t){return r(2,t,function(r){return function(n){return t(r,n)}})}function t(u){return r(3,u,function(t){return function(r){return function(n){return u(t,r,n)}}})}function u(e){return r(4,e,function(u){return function(t){return function(r){return function(n){return e(u,t,r,n)}}}})}function e(i){return r(5,i,function(e){return function(u){return function(t){return function(r){return function(n){return i(e,u,t,r,n)}}}}})}function i(o){return r(6,o,function(i){return function(e){return function(u){return function(t){return function(r){return function(n){return o(i,e,u,t,r,n)}}}}}})}function o(f){return r(7,f,function(o){return function(i){return function(e){return function(u){return function(t){return function(r){return function(n){return f(o,i,e,u,t,r,n)}}}}}}})}function f(a){return r(8,a,function(f){return function(o){return function(i){return function(e){return function(u){return function(t){return function(r){return function(n){return a(f,o,i,e,u,t,r,n)}}}}}}}})}function c(c){return r(9,c,function(a){return function(f){return function(o){return function(i){return function(e){return function(u){return function(t){return function(r){return function(n){return c(a,f,o,i,e,u,t,r,n)}}}}}}}}})}function g(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function p(n,r,t,u){return 3===n.a?n.f(r,t,u):n(r)(t)(u)}function s(n,r,t,u,e){return 4===n.a?n.f(r,t,u,e):n(r)(t)(u)(e)}function v(n,r,t,u,e,i){return 5===n.a?n.f(r,t,u,e,i):n(r)(t)(u)(e)(i)}function b(n,r,t,u,e,i,o){return 6===n.a?n.f(r,t,u,e,i,o):n(r)(t)(u)(e)(i)(o)}var l=t(function(n,r,t){for(var u=Array(n),e=0;e<n;e++)u[e]=t(r+e);return u}),d=a(function(n,r){for(var t=Array(n),u=0;u<n&&r.b;u++)t[u]=r.a,r=r.b;return t.length=u,x(t,r)}),h=(a(function(n,r){return r[n]}),t(function(n,r,t){for(var u=t.length,e=Array(u),i=0;i<u;i++)e[i]=t[i];return e[n]=r,e}),a(function(n,r){for(var t=r.length,u=Array(t+1),e=0;e<t;e++)u[e]=r[e];return u[t]=n,u}),t(function(n,r,t){for(var u=t.length,e=0;e<u;e++)r=g(n,t[e],r);return r}),t(function(n,r,t){for(var u=t.length-1;0<=u;u--)r=g(n,t[u],r);return r}));a(function(n,r){for(var t=r.length,u=Array(t),e=0;e<t;e++)u[e]=n(r[e]);return u}),t(function(n,r,t){for(var u=t.length,e=Array(u),i=0;i<u;i++)e[i]=g(n,r+i,t[i]);return e}),t(function(n,r,t){return t.slice(n,r)}),t(function(n,r,t){var u=r.length,e=n-u;t.length<e&&(e=t.length);for(var i=Array(u+e),o=0;o<u;o++)i[o]=r[o];for(o=0;o<e;o++)i[o+u]=t[o];return i}),a(function(n,r){return r}),a(function(n,r){return console.log(n+": <internals>"),r});function $(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}function m(n,r){for(var t,u=[],e=y(n,r,0,u);e&&(t=u.pop());e=y(t.a,t.b,0,u));return e}function y(n,r,t,u){if(100<t)return u.push(x(n,r)),!0;if(n===r)return!0;if("object"!=typeof n||null===n||null===r)return"function"==typeof n&&$(5),!1;for(var e in n.$<0&&(n=bt(n),r=bt(r)),n)if(!y(n[e],r[e],t+1,u))return!1;return!0}a(m),a(function(n,r){return!m(n,r)});function A(n,r,t){if("object"!=typeof n)return n===r?0:n<r?-1:1;if(void 0===n.$)return(t=A(n.a,r.a))?t:(t=A(n.b,r.b))?t:A(n.c,r.c);for(;n.b&&r.b&&!(t=A(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}a(function(n,r){return A(n,r)<0}),a(function(n,r){return A(n,r)<1}),a(function(n,r){return 0<A(n,r)}),a(function(n,r){return 0<=A(n,r)}),a(function(n,r){var t=A(n,r);return t<0?ot:t?ct:it});var j=0;function x(n,r){return{a:n,b:r}}function k(n){return n}function w(n,r){var t={};for(var u in n)t[u]=n[u];for(var u in r)t[u]=r[u];return t}a(C);function C(n,r){if("string"==typeof n)return n+r;if(!n.b)return r;var t=E(n.a,r);n=n.b;for(var u=t;n.b;n=n.b)u=u.b=E(n.a,r);return t}var N={$:0};function E(n,r){return{$:1,a:n,b:r}}var _=a(E);function T(n){for(var r=N,t=n.length;t--;)r=E(n[t],r);return r}function L(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}var O=t(function(n,r,t){for(var u=[];r.b&&t.b;r=r.b,t=t.b)u.push(g(n,r.a,t.a));return T(u)}),I=(u(function(n,r,t,u){for(var e=[];r.b&&t.b&&u.b;r=r.b,t=t.b,u=u.b)e.push(p(n,r.a,t.a,u.a));return T(e)}),e(function(n,r,t,u,e){for(var i=[];r.b&&t.b&&u.b&&e.b;r=r.b,t=t.b,u=u.b,e=e.b)i.push(s(n,r.a,t.a,u.a,e.a));return T(i)}),i(function(n,r,t,u,e,i){for(var o=[];r.b&&t.b&&u.b&&e.b&&i.b;r=r.b,t=t.b,u=u.b,e=e.b,i=i.b)o.push(v(n,r.a,t.a,u.a,e.a,i.a));return T(o)}),a(function(t,n){return T(L(n).sort(function(n,r){return A(t(n),t(r))}))}),a(function(u,n){return T(L(n).sort(function(n,r){var t=g(u,n,r);return t===it?0:t===ot?-1:1}))}),a(function(n,r){return n+r}));a(function(n,r){return n+r});a(function(n,r){for(var t=r.length,u=Array(t),e=0;e<t;){var i=r.charCodeAt(e);i<55296||56319<i?(u[e]=n(k(r[e])),e++):(u[e]=n(k(r[e]+r[e+1])),e+=2)}return u.join("")}),a(function(n,r){for(var t=[],u=r.length,e=0;e<u;){var i=r[e],o=r.charCodeAt(e);e++,o<55296||56319<o||(i+=r[e],e++),n(k(i))&&t.push(i)}return t.join("")});t(function(n,r,t){for(var u=t.length,e=0;e<u;){var i=t[e],o=t.charCodeAt(e);e++,o<55296||56319<o||(i+=t[e],e++),r=g(n,k(i),r)}return r});var P=t(function(n,r,t){for(var u=t.length;u--;){var e=t[u],i=t.charCodeAt(u);i<56320||57343<i||(e=t[--u]+e),r=g(n,k(e),r)}return r}),S=a(function(n,r){return r.split(n)}),B=a(function(n,r){return r.join(n)}),F=t(function(n,r,t){return t.slice(n,r)});a(function(n,r){for(var t=r.length;t--;){var u=r[t],e=r.charCodeAt(t);if(e<56320||57343<e||(u=r[--t]+u),n(k(u)))return!0}return!1});var R=a(function(n,r){for(var t=r.length;t--;){var u=r[t],e=r.charCodeAt(t);if(e<56320||57343<e||(u=r[--t]+u),!n(k(u)))return!1}return!0}),D=a(function(n,r){return!!~r.indexOf(n)}),q=a(function(n,r){return 0==r.indexOf(n)}),J=(a(function(n,r){return n.length<=r.length&&r.lastIndexOf(n)==r.length-n.length}),a(function(n,r){var t=n.length;if(t<1)return N;for(var u=0,e=[];-1<(u=r.indexOf(n,u));)e.push(u),u+=t;return T(e)}));function H(n){return n+""}a(function(n,r){return n+r}),a(function(n,r){return n-r}),a(function(n,r){return n*r}),a(function(n,r){return n/r}),a(function(n,r){return n/r|0}),a(Math.pow),a(function(n,r){return r%n}),a(function(n,r){var t=r%n;return 0===n?$(11):0<t&&n<0||t<0&&0<n?t+n:t}),a(Math.atan2);var M=Math.ceil,z=Math.floor,W=Math.log,G=isNaN;a(function(n,r){return n&&r}),a(function(n,r){return n||r}),a(function(n,r){return n!==r});a(function(n,r){return n&r}),a(function(n,r){return n|r}),a(function(n,r){return n^r});a(function(n,r){return r<<n}),a(function(n,r){return r>>n}),a(function(n,r){return r>>>n});var Y=a(function(n,r){var t="g";n.aC&&(t+="m"),n.ao&&(t+="i");try{return ut(RegExp(r,t))}catch(n){return et}}),K=(a(function(n,r){return null!==r.match(n)}),t(function(n,r,t){for(var u,e=[],i=0,o=t,f=r.lastIndex,a=-1;i++<n&&(u=r.exec(o))&&a!=r.lastIndex;){for(var c=u.length-1,v=Array(c);0<c;){var b=u[c];v[--c]=b?ut(b):et}e.push(s(Kt,u[0],u.index,i,T(v))),a=r.lastIndex}return r.lastIndex=f,T(e)}));u(function(e,n,i,r){var o=0;return r.replace(n,function(n){if(o++>=e)return n;for(var r=arguments.length-3,t=Array(r);0<r;){var u=arguments[r];t[--r]=u?ut(u):et}return i(s(Kt,n,arguments[arguments.length-2],o,T(t)))})}),t(function(n,r,t){for(var u=t,e=[],i=r.lastIndex,o=r.lastIndex;n--;){var f=r.exec(u);if(!f)break;e.push(u.slice(i,f.index)),i=r.lastIndex}return e.push(u.slice(i)),r.lastIndex=o,T(e)});function Q(n){return{$:2,b:n}}Q(function(n){return"number"!=typeof n?vn("an INT",n):-2147483647<n&&n<2147483647&&(0|n)===n?$u(n):!isFinite(n)||n%1?vn("an INT",n):$u(n)});var U=Q(function(n){return"boolean"==typeof n?$u(n):vn("a BOOL",n)}),V=(Q(function(n){return"number"==typeof n?$u(n):vn("a FLOAT",n)}),Q(function(n){return $u(dn(n))}),Q(function(n){return"string"==typeof n?$u(n):n instanceof String?$u(n+""):vn("a STRING",n)}));var X=a(function(n,r){return{$:6,d:n,b:r}}),Z=a(function(n,r){return{$:7,e:n,b:r}});function nn(n,r){return{$:9,f:n,g:r}}var rn=a(function(n,r){return{$:10,b:r,h:n}});var tn=a(function(n,r){return nn(n,[r])}),un=t(function(n,r,t){return nn(n,[r,t])}),en=(u(function(n,r,t,u){return nn(n,[r,t,u])}),e(function(n,r,t,u,e){return nn(n,[r,t,u,e])}),i(function(n,r,t,u,e,i){return nn(n,[r,t,u,e,i])}),o(function(n,r,t,u,e,i,o){return nn(n,[r,t,u,e,i,o])}),f(function(n,r,t,u,e,i,o,f){return nn(n,[r,t,u,e,i,o,f])}),c(function(n,r,t,u,e,i,o,f,a){return nn(n,[r,t,u,e,i,o,f,a])}),a(function(n,r){try{return on(n,JSON.parse(r))}catch(n){return pu(g(yu,"This is not valid JSON! "+n.message,dn(r)))}}),a(function(n,r){return on(n,hn(r))}));function on(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?$u(n.c):vn("null",r);case 3:return an(r)?fn(n.b,r,T):vn("a LIST",r);case 4:return an(r)?fn(n.b,r,cn):vn("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return vn("an OBJECT with a field named `"+t+"`",r);var u=on(n.b,r[t]);return mu(u)?u:pu(g(Au,t,u.a));case 7:var e=n.e;if(!an(r))return vn("an ARRAY",r);if(r.length<=e)return vn("a LONGER array. Need index "+e+" but only see "+r.length+" entries",r);u=on(n.b,r[e]);return mu(u)?u:pu(g(ju,e,u.a));case 8:if("object"!=typeof r||null===r||an(r))return vn("an OBJECT",r);var i=N;for(var o in r)if(r.hasOwnProperty(o)){u=on(n.b,r[o]);if(!mu(u))return pu(g(Au,o,u.a));i=E(x(o,u.a),i)}return $u(zt(i));case 9:for(var f=n.f,a=n.g,c=0;c<a.length;c++){u=on(a[c],r);if(!mu(u))return u;f=f(u.a)}return $u(f);case 10:u=on(n.b,r);return mu(u)?on(n.h(u.a),r):u;case 11:for(var v=N,b=n.g;b.b;b=b.b){u=on(b.a,r);if(mu(u))return u;v=E(u.a,v)}return pu(xu(zt(v)));case 1:return pu(g(yu,n.a,dn(r)));case 0:return $u(n.a)}}function fn(n,r,t){for(var u=r.length,e=Array(u),i=0;i<u;i++){var o=on(n,r[i]);if(!mu(o))return pu(g(ju,i,o.a));e[i]=o.a}return $u(t(e))}function an(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function cn(r){return g(gu,r.length,function(n){return r[n]})}function vn(n,r){return pu(g(yu,"Expecting "+n,dn(r)))}function bn(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return bn(n.b,r.b);case 6:return n.d===r.d&&bn(n.b,r.b);case 7:return n.e===r.e&&bn(n.b,r.b);case 9:return n.f===r.f&&sn(n.g,r.g);case 10:return n.h===r.h&&bn(n.b,r.b);case 11:return sn(n.g,r.g)}}function sn(n,r){var t=n.length;if(t!==r.length)return!1;for(var u=0;u<t;u++)if(!bn(n[u],r[u]))return!1;return!0}var ln=a(function(n,r){return JSON.stringify(hn(r),null,n)+""});function dn(n){return n}function hn(n){return n}t(function(n,r,t){return t[n]=hn(r),t});dn(null);function gn(n){return{$:0,a:n}}function pn(n){return{$:2,b:n,c:null}}var $n=a(function(n,r){return{$:3,b:n,d:r}});a(function(n,r){return{$:4,b:n,d:r}});var mn=0;function yn(n){var r={$:0,e:mn++,f:n,g:null,h:[]};return Cn(r),r}function An(r){return pn(function(n){n(gn(yn(r)))})}function jn(n,r){n.h.push(r),Cn(n)}var xn=a(function(r,t){return pn(function(n){jn(r,t),n(gn(j))})});var kn=!1,wn=[];function Cn(n){if(wn.push(n),!kn){for(kn=!0;n=wn.shift();)Nn(n);kn=!1}}function Nn(r){for(;r.f;){var n=r.f.$;if(0===n||1===n){for(;r.g&&r.g.$!==n;)r.g=r.g.i;if(!r.g)return;r.f=r.g.b(r.f.a),r.g=r.g.i}else{if(2===n)return void(r.f.c=r.f.b(function(n){r.f=n,Cn(r)}));if(5===n){if(0===r.h.length)return;r.f=r.f.b(r.h.shift())}else r.g={$:3===n?0:1,b:r.f.b,i:r.g},r.f=r.f.d}}}u(function(n,r,t,u){return En(r,u,n.bd,n.bB,n.bx,function(){return function(){}})});function En(n,r,t,u,e,i){var o=g(en,n,dn(r?r.flags:void 0));mu(o)||$(2);var f={},a=(o=t(o.a)).a,c=i(b,a),v=function(n,r){var t;for(var u in _n){var e=_n[u];e.a&&((t=t||{})[u]=e.a(u,r)),n[u]=Tn(e,r)}return t}(f,b);function b(n,r){o=g(u,n,a),c(a=o.a,r),In(f,o.b,e(a))}return In(f,o.b,e(a)),v?{ports:v}:{}}var _n={};function Tn(n,r){var u={g:r,h:void 0},e=n.c,i=n.d,o=n.e,f=n.f;return u.h=yn(g($n,function n(t){return g($n,n,function(n){return{$:5,b:n}}(function(n){var r=n.a;return 0===n.$?p(i,u,r,t):o&&f?s(e,u,r.i,r.j,t):p(e,u,o?r.i:r.j,t)}))},n.b))}var Ln=a(function(r,t){return pn(function(n){r.g(t),n(gn(j))})});a(function(n,r){return g(xn,n.h,{$:0,a:r})});function On(r){return function(n){return{$:1,k:r,l:n}}}a(function(n,r){return{$:3,n:n,o:r}});function In(n,r,t){var u={};for(var e in Pn(!0,r,u,null),Pn(!1,t,u,null),n)jn(n[e],{$:"fx",a:u[e]||{i:N,j:N}})}function Pn(n,r,t,u){switch(r.$){case 1:var e=r.k,i=function(n,r,t,u){return g(n?_n[r].e:_n[r].f,function(n){for(var r=t;r;r=r.q)n=r.p(n);return n},u)}(n,e,u,r.l);return void(t[e]=function(n,r,t){return t=t||{i:N,j:N},n?t.i=E(r,t.i):t.j=E(r,t.j),t}(n,i,t[e]));case 2:for(var o=r.m;o.b;o=o.b)Pn(n,o.a,t,u);return;case 3:return void Pn(n,r.o,t,{p:r.n,q:u})}}function Sn(n){_n[n]&&$(3)}var Bn=a(function(n,r){return r});function Fn(n){var i=[],o=_n[n].r,f=function(t){return pn(function(n){var r=setTimeout(function(){n(gn(j))},t);return function(){clearTimeout(r)}})}(0);return _n[n].b=f,_n[n].c=t(function(n,r){for(;r.b;r=r.b)for(var t=i,u=hn(o(r.a)),e=0;e<t.length;e++)t[e](u);return f}),{subscribe:function(n){i.push(n)},unsubscribe:function(n){var r=(i=i.slice()).indexOf(n);r<0||i.splice(r,1)}}}var Rn;a(function(r,t){return function(n){return r(t(n))}});var Dn="undefined"!=typeof document?document:{};function qn(n,r){n.appendChild(r)}u(function(n,r,t,u){var e=u.node;return e.parentNode.replaceChild(Xn(n,function(){}),e),{}});function Jn(n){return{$:0,a:n}}var Hn=a(function(i,o){return a(function(n,r){for(var t=[],u=0;r.b;r=r.b){var e=r.a;u+=e.b||0,t.push(e)}return u+=t.length,{$:1,c:o,d:Un(n),e:t,f:i,b:u}})})(void 0);a(function(i,o){return a(function(n,r){for(var t=[],u=0;r.b;r=r.b){var e=r.a;u+=e.b.b||0,t.push(e)}return u+=t.length,{$:2,c:o,d:Un(n),e:t,f:i,b:u}})})(void 0);a(function(n,r){return{$:4,j:n,k:r,b:1+(r.b||0)}});function Mn(n,r){return{$:5,l:n,m:r,k:void 0}}a(function(n,r){return Mn([n,r],function(){return n(r)})}),t(function(n,r,t){return Mn([n,r,t],function(){return g(n,r,t)})}),u(function(n,r,t,u){return Mn([n,r,t,u],function(){return p(n,r,t,u)})}),e(function(n,r,t,u,e){return Mn([n,r,t,u,e],function(){return s(n,r,t,u,e)})}),i(function(n,r,t,u,e,i){return Mn([n,r,t,u,e,i],function(){return v(n,r,t,u,e,i)})}),o(function(n,r,t,u,e,i,o){return Mn([n,r,t,u,e,i,o],function(){return b(n,r,t,u,e,i,o)})}),f(function(n,r,t,u,e,i,o,f){return Mn([n,r,t,u,e,i,o,f],function(){return function(n,r,t,u,e,i,o,f){return 7===n.a?n.f(r,t,u,e,i,o,f):n(r)(t)(u)(e)(i)(o)(f)}(n,r,t,u,e,i,o,f)})}),c(function(n,r,t,u,e,i,o,f,a){return Mn([n,r,t,u,e,i,o,f,a],function(){return function(n,r,t,u,e,i,o,f,a){return 8===n.a?n.f(r,t,u,e,i,o,f,a):n(r)(t)(u)(e)(i)(o)(f)(a)}(n,r,t,u,e,i,o,f,a)})});var zn=a(function(n,r){return{$:"a0",n:n,o:r}}),Wn=(a(function(n,r){return{$:"a1",n:n,o:r}}),a(function(n,r){return{$:"a2",n:n,o:r}})),Gn=a(function(n,r){return{$:"a3",n:n,o:r}});t(function(n,r,t){return{$:"a4",n:r,o:{f:n,o:t}}});a(function(n,r){return"a0"===r.$?g(zn,r.n,function(n,r){var t=Yu(r);return{$:r.$,a:t?p(Wu,t<3?Kn:Qn,Gu(n),r.a):g(zu,n,r.a)}}(n,r.o)):r});var Yn,Kn=a(function(n,r){return x(n(r.a),r.b)}),Qn=a(function(n,r){return{v:n(r.v),ai:r.ai,ag:r.ag}});function Un(n){for(var r={};n.b;n=n.b){var t=n.a,u=t.$,e=t.n,i=t.o;if("a2"!==u){var o=r[u]||(r[u]={});"a3"===u&&"class"===e?Vn(o,e,i):o[e]=i}else"className"===e?Vn(r,e,hn(i)):r[e]=hn(i)}return r}function Vn(n,r,t){var u=n[r];n[r]=u?u+" "+t:t}function Xn(n,r){var t=n.$;if(5===t)return Xn(n.k||(n.k=n.m()),r);if(0===t)return Dn.createTextNode(n.a);if(4===t){for(var u=n.k,e=n.j;4===u.$;)"object"!=typeof e?e=[e,u.j]:e.push(u.j),u=u.k;var i={j:e,p:r};return(o=Xn(u,i)).elm_event_node_ref=i,o}if(3===t)return Zn(o=n.h(n.g),r,n.d),o;var o=n.f?Dn.createElementNS(n.f,n.c):Dn.createElement(n.c);Rn&&"a"==n.c&&o.addEventListener("click",Rn(o)),Zn(o,r,n.d);for(var f=n.e,a=0;a<f.length;a++)qn(o,Xn(1===t?f[a]:f[a].b,r));return o}function Zn(n,r,t){for(var u in t){var e=t[u];"a1"===u?nr(n,e):"a0"===u?ur(n,r,e):"a3"===u?rr(n,e):"a4"===u?tr(n,e):("value"!==u&&"checked"!==u||n[u]!==e)&&(n[u]=e)}}function nr(n,r){var t=n.style;for(var u in r)t[u]=r[u]}function rr(n,r){for(var t in r){var u=r[t];void 0!==u?n.setAttribute(t,u):n.removeAttribute(t)}}function tr(n,r){for(var t in r){var u=r[t],e=u.f,i=u.o;void 0!==i?n.setAttributeNS(e,t,i):n.removeAttributeNS(e,t)}}function ur(n,r,t){var u=n.elmFs||(n.elmFs={});for(var e in t){var i=t[e],o=u[e];if(i){if(o){if(o.q.$===i.$){o.q=i;continue}n.removeEventListener(e,o)}o=er(r,i),n.addEventListener(e,o,Yn&&{passive:Yu(i)<2}),u[e]=o}else n.removeEventListener(e,o),u[e]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){Yn=!0}}))}catch(n){}function er(v,n){function b(n){var r=b.q,t=on(r.a,n);if(mu(t)){for(var u,e=Yu(r),i=t.a,o=e?e<3?i.a:i.v:i,f=1==e?i.b:3==e&&i.ai,a=(f&&n.stopPropagation(),(2==e?i.b:3==e&&i.ag)&&n.preventDefault(),v);u=a.j;){if("function"==typeof u)o=u(o);else for(var c=u.length;c--;)o=u[c](o);a=a.p}a(o,f)}}return b.q=n,b}function ir(n,r){return n.$==r.$&&bn(n.a,r.a)}function or(n,r){var t=[];return ar(n,r,t,0),t}function fr(n,r,t,u){var e={$:r,r:t,s:u,t:void 0,u:void 0};return n.push(e),e}function ar(n,r,t,u){if(n!==r){var e=n.$,i=r.$;if(e!==i){if(1!==e||2!==i)return void fr(t,0,u,r);r=function(n){for(var r=n.e,t=r.length,u=Array(t),e=0;e<t;e++)u[e]=r[e].b;return{$:1,c:n.c,d:n.d,e:u,f:n.f,b:n.b}}(r),i=1}switch(i){case 5:for(var o=n.l,f=r.l,a=o.length,c=a===f.length;c&&a--;)c=o[a]===f[a];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return ar(n.k,r.k,v,0),void(0<v.length&&fr(t,1,u,v));case 4:for(var b=n.j,s=r.j,l=!1,d=n.k;4===d.$;)l=!0,"object"!=typeof b?b=[b,d.j]:b.push(d.j),d=d.k;for(var h=r.k;4===h.$;)l=!0,"object"!=typeof s?s=[s,h.j]:s.push(h.j),h=h.k;return l&&b.length!==s.length?void fr(t,0,u,r):((l?function(n,r){for(var t=0;t<n.length;t++)if(n[t]!==r[t])return!1;return!0}(b,s):b===s)||fr(t,2,u,s),void ar(d,h,t,u+1));case 0:return void(n.a!==r.a&&fr(t,3,u,r.a));case 1:return void cr(n,r,t,u,br);case 2:return void cr(n,r,t,u,sr);case 3:if(n.h!==r.h)return void fr(t,0,u,r);var g=vr(n.d,r.d);g&&fr(t,4,u,g);var p=r.i(n.g,r.g);return void(p&&fr(t,5,u,p))}}}function cr(n,r,t,u,e){if(n.c===r.c&&n.f===r.f){var i=vr(n.d,r.d);i&&fr(t,4,u,i),e(n,r,t,u)}else fr(t,0,u,r)}function vr(n,r,t){var u;for(var e in n)if("a1"!==e&&"a0"!==e&&"a3"!==e&&"a4"!==e)if(e in r){var i=n[e],o=r[e];i===o&&"value"!==e&&"checked"!==e||"a0"===t&&ir(i,o)||((u=u||{})[e]=o)}else(u=u||{})[e]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[e].f,o:void 0}:"string"==typeof n[e]?"":null;else{var f=vr(n[e],r[e]||{},e);f&&((u=u||{})[e]=f)}for(var a in r)a in n||((u=u||{})[a]=r[a]);return u}function br(n,r,t,u){var e=n.e,i=r.e,o=e.length,f=i.length;f<o?fr(t,6,u,{v:f,i:o-f}):o<f&&fr(t,7,u,{v:o,e:i});for(var a=o<f?o:f,c=0;c<a;c++){var v=e[c];ar(v,i[c],t,++u),u+=v.b||0}}function sr(n,r,t,u){for(var e=[],i={},o=[],f=n.e,a=r.e,c=f.length,v=a.length,b=0,s=0,l=u;b<c&&s<v;){var d=(C=f[b]).a,h=(N=a[s]).a,g=C.b,p=N.b,$=void 0,m=void 0;if(d!==h){var y=f[b+1],A=a[s+1];if(y){var j=y.a,x=y.b;m=h===j}if(A){var k=A.a,w=A.b;$=d===k}if($&&m)ar(g,w,e,++l),dr(i,e,d,p,s,o),l+=g.b||0,hr(i,e,d,x,++l),l+=x.b||0,b+=2,s+=2;else if($)l++,dr(i,e,h,p,s,o),ar(g,w,e,l),l+=g.b||0,b+=1,s+=2;else if(m)hr(i,e,d,g,++l),l+=g.b||0,ar(x,p,e,++l),l+=x.b||0,b+=2,s+=1;else{if(!y||j!==k)break;hr(i,e,d,g,++l),dr(i,e,h,p,s,o),l+=g.b||0,ar(x,w,e,++l),l+=x.b||0,b+=2,s+=2}}else ar(g,p,e,++l),l+=g.b||0,b++,s++}for(;b<c;){var C;hr(i,e,(C=f[b]).a,g=C.b,++l),l+=g.b||0,b++}for(;s<v;){var N,E=E||[];dr(i,e,(N=a[s]).a,N.b,void 0,E),s++}(0<e.length||0<o.length||E)&&fr(t,8,u,{w:e,x:o,y:E})}var lr="_elmW6BL";function dr(n,r,t,u,e,i){var o=n[t];if(!o)return i.push({r:e,A:o={c:0,z:u,r:e,s:void 0}}),void(n[t]=o);if(1===o.c){i.push({r:e,A:o}),o.c=2;var f=[];return ar(o.z,u,f,o.r),o.r=e,void(o.s.s={w:f,A:o})}dr(n,r,t+lr,u,e,i)}function hr(n,r,t,u,e){var i=n[t];if(i){if(0===i.c){i.c=2;var o=[];return ar(u,i.z,o,e),void fr(r,9,e,{w:o,A:i})}hr(n,r,t+lr,u,e)}else{var f=fr(r,9,e,void 0);n[t]={c:1,z:u,r:e,s:f}}}function gr(n,r,t,u){!function n(r,t,u,e,i,o,f){var a=u[e];var c=a.r;for(;c===i;){var v=a.$;if(1===v)gr(r,t.k,a.s,f);else if(8===v){a.t=r,a.u=f;var b=a.s.w;0<b.length&&n(r,t,b,0,i,o,f)}else if(9===v){a.t=r,a.u=f;var s=a.s;if(s){s.A.s=r;var b=s.w;0<b.length&&n(r,t,b,0,i,o,f)}}else a.t=r,a.u=f;if(!(a=u[++e])||(c=a.r)>o)return e}var l=t.$;if(4===l){for(var d=t.k;4===d.$;)d=d.k;return n(r,d,u,e,i+1,o,r.elm_event_node_ref)}var h=t.e;var g=r.childNodes;for(var p=0;p<h.length;p++){var $=1===l?h[p]:h[p].b,m=++i+($.b||0);if(i<=c&&c<=m&&(e=n(g[p],$,u,e,i,m,f),!(a=u[e])||(c=a.r)>o))return e;i=m}return e}(n,r,t,0,0,r.b,u)}function pr(n,r,t,u){return 0===t.length?n:(gr(n,r,t,u),$r(n,t))}function $r(n,r){for(var t=0;t<r.length;t++){var u=r[t],e=u.t,i=mr(e,u);e===n&&(n=i)}return n}function mr(n,r){switch(r.$){case 0:return function(n,r,t){var u=n.parentNode,e=Xn(r,t);e.elm_event_node_ref||(e.elm_event_node_ref=n.elm_event_node_ref);u&&e!==n&&u.replaceChild(e,n);return e}(n,r.s,r.u);case 4:return Zn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return $r(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,u=0;u<t.i;u++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var e=(t=r.s).e,i=n.childNodes[u=t.v];u<e.length;u++)n.insertBefore(Xn(e[u],r.u),i);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var o=t.A;return void 0!==o.r&&n.parentNode.removeChild(n),o.s=$r(n,t.w),n;case 8:return function(n,r){var t=r.s,u=function(n,r){if(!n)return;for(var t=Dn.createDocumentFragment(),u=0;u<n.length;u++){var e=n[u].A;qn(t,2===e.c?e.s:Xn(e.z,r.u))}return t}(t.y,r);n=$r(n,t.w);for(var e=t.x,i=0;i<e.length;i++){var o=e[i],f=o.A,a=2===f.c?f.s:Xn(f.z,r.u);n.insertBefore(a,n.childNodes[o.r])}u&&qn(n,u);return n}(n,r);case 5:return r.s(n);default:$(10)}}function yr(n){if(3===n.nodeType)return Jn(n.textContent);if(1!==n.nodeType)return Jn("");for(var r=N,t=n.attributes,u=t.length;u--;){var e=t[u];r=E(g(Gn,e.name,e.value),r)}var i=n.tagName.toLowerCase(),o=N,f=n.childNodes;for(u=f.length;u--;)o=E(yr(f[u]),o);return p(Hn,i,r,o)}var Ar=u(function(r,n,t,f){return En(n,f,r.bd,r.bB,r.bx,function(u,n){var e=r.bD,i=f.node,o=yr(i);return xr(n,function(n){var r=e(n),t=or(o,r);i=pr(i,o,t,u),o=r})})}),jr=(u(function(r,n,t,u){return En(n,u,r.bd,r.bB,r.bx,function(e,n){var i=r.R&&r.R(e),o=r.bD,f=Dn.title,a=Dn.body,c=yr(a);return xr(n,function(n){Rn=i;var r=o(n),t=Hn("body")(N)(r.a_),u=or(c,t);a=pr(a,c,u,e),c=t,Rn=0,f!==r.bA&&(Dn.title=f=r.bA)})})}),"undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});function xr(t,u){u(t);var e=0;function i(){e=1===e?0:(jr(i),u(t),1)}return function(n,r){t=n,r?(u(t),2===e&&(e=1)):(0===e&&jr(i),e=2)}}a(function(n,r){return g(ni,qe,pn(function(){r&&history.go(r),n()}))}),a(function(n,r){return g(ni,qe,pn(function(){history.pushState({},"",r),n()}))}),a(function(n,r){return g(ni,qe,pn(function(){history.replaceState({},"",r),n()}))});var kr={addEventListener:function(){},removeEventListener:function(){}},wr=("undefined"!=typeof document&&document,"undefined"!=typeof window?window:kr);t(function(r,t,u){return An(pn(function(){function n(n){yn(u(n))}return r.addEventListener(t,n,Yn&&{passive:!0}),function(){r.removeEventListener(t,n)}}))}),a(function(n,r){var t=on(n,r);return mu(t)?ut(t.a):et});function Cr(t,u){return pn(function(r){jr(function(){var n=document.getElementById(t);r(n?gn(u(n)):function(n){return{$:1,a:n}}(De(t)))})})}a(function(r,n){return Cr(n,function(n){return n[r](),j})});a(function(n,r){return function(r){return pn(function(n){jr(function(){n(gn(r()))})})}(function(){return wr.scroll(n,r),j})});t(function(n,r,t){return Cr(n,function(n){return n.scrollLeft=r,n.scrollTop=t,j})});function Nr(n){return{$:0,a:n}}function Er(n){return n<0?-n:n}function _r(n){return g(xt,n,"")}function Tr(n){var r=g(Pt,".",n);return r.b?x(r.a,r.b.b?r.b.a:"0"):x("0","0")}function Lr(n){return{$:1,a:n}}function Or(n){if(n.b){return ut(n.a)}return et}function Ir(n){var r=n.g;if(r.$){var t=r.a.x,u=r.a.b;return{b:u,k:r.a.y/t*100,j:u/(u+t)*100}}return r.a}function Pr(n){return n}function Sr(n){var r=gt(n);return 97<=r&&r<=122}function Br(n){var r=gt(n);return r<=90&&65<=r}function Fr(n){return Sr(n)||Br(n)}function Rr(n){return Sr(n)||Br(n)||function(n){var r=gt(n);return r<=57&&48<=r}(n)}function Dr(n){return p(Mt,a(function(n,r){return r+1}),0,n)}function qr(n){return g(Tu,"\n    ",g(Pt,"\n",n))}function Jr(n){return Ku(T([Vu(n)]))}function Hr(n){return T([n])}function Mr(n){if(n.b){return ut(n.b)}return et}function zr(r){var n=g(Yt,function(n){return 10*n},g(Cu,0,11)),t=ge(r),u=g(Yt,t,n),e=function(n){var r=a(function(n,r){var t=n.b,u=r.b;return x(g(at,n.a,r.a),g(at,t,u))});return p(Gt,r,x(N,N),n)}(g(pe,function(n){var r=n.a;return 0<=r&&r<140},p(ku,$e,u,n))),i=e.a,o=e.b,f=T([g(Yt,function(n){return n/100*Ir(r).b},i),i,o]);return g(he,T(["Assignment grade (points)","Assignment grade (%)","Total grade"]),f)}function Wr(n){return{$:2,a:n}}function Gr(n){return{$:0,a:n}}function Yr(n){return{$:1,a:n}}function Kr(n){return{$:4,a:n}}function Qr(n){return{$:3,a:n}}function Ur(n){return g(Uu,"step",n)}function Vr(n){return x(n,!0)}function Xr(n){return g(Le,"input",g(zu,Vr,g(zu,n,Se)))}var Zr,nt,rt,tt=t(function(n,r,t){return r(n(t))}),ut=function(n){return{$:0,a:n}},et={$:1},it=1,ot=0,ft=h,at=(t(function(t,n,r){var u=r.c,e=r.d,i=a(function(n,r){return p(ft,n.$?t:i,r,n.a)});return p(ft,i,p(ft,t,n,e),u)}),_),ct=2,vt=t(function(n,r,t){for(;;){if(-2===t.$)return r;var u=t.d,e=n,i=p(n,t.b,t.c,p(vt,n,r,t.e));n=e,r=i,t=u}}),bt=function(n){return p(vt,t(function(n,r,t){return g(at,x(n,r),t)}),N,n)},st=H,lt=(a(function(n,r){return r(n)}),a(function(n,r){return r.$?n:r.a})),dt=function(n){if(0===n.length||/[\sxbo]/.test(n))return et;var r=+n;return r==r?ut(r):et},ht=t(function(n,r,t){return g(lt,NaN,dt(g(n,r,t)))}),gt=function(n){var r=n.charCodeAt(0);return r<55296||56319<r?r:1024*(r-55296)+n.charCodeAt(1)-56320+65536},pt=function(n){var r=n.charCodeAt(0);return r?ut(r<55296||56319<r?x(k(n[0]),n.slice(1)):x(k(n[0]+n[1]),n.slice(2))):et},$t=function(n){return n===1/0||n===-1/0},mt=G,yt=a(function(n,r){return 0<A(n,r)?n:r}),At=a(function(n,r){return r.$?et:ut(n(r.a))}),jt=function(n){return n.length},xt=I,kt=(a(function(n,r){return n(r)}),t(function(n,r,t){return 0<n?p(kt,n>>1,C(r,r),1&n?C(t,r):t):t})),wt=a(function(n,r){return p(kt,n,r,"")}),Ct=t(function(n,r,t){return C(t,g(wt,n-jt(t),_r(r)))}),Nt=function(n){for(var r=n.length,t=Array(r),u=0;u<r;){var e=n.charCodeAt(u);e<55296||56319<e?t[r-u]=n[u]:(t[r-u]=n[u+1],t[r-++u]=n[u-1]),u++}return t.join("")},Et=F,_t=a(function(n,r){for(;;){if(!r.b)return!1;var t=r.b;if(n(r.a))return!0;n=n,r=t}}),Tt=P,Lt=a(function(n,r){var t=g(_t,function(n){return"0"!==n&&"."!==n},function(n){return p(Tt,at,N,n)}(r));return C(n&&t?"-":"",r)}),Ot=function(n){return k(n<0||1114111<n?"�":65535<n?String.fromCharCode(55296+Math.floor((n-=65536)/1024),n%1024+56320):String.fromCharCode(n))},It=function(n){var r=n.a,t=n.b;if("9"===r){var u=pt(t);return 1===u.$?"01":g(xt,"0",It(u.a))}var e=gt(r);return 48<=e&&e<57?g(xt,Ot(e+1),t):"0"},Pt=a(function(n,r){return T(g(S,n,r))}),St=a(function(n,r){return n<1?r:p(Et,n,jt(r),r)}),Bt=q,Ft=function(n){for(var r=0,t=n.charCodeAt(0),u=43==t||45==t?1:0,e=u;e<n.length;++e){var i=n.charCodeAt(e);if(i<48||57<i)return et;r=10*r+i-48}return e==u?et:ut(45==t?-r:r)},Rt=a(function(n,r){var t=r.b;return x(n(r.a),t)}),Dt=ht(t(function(n,r,t){if($t(t)||mt(t))return st(t);var u=t<0,e=Tr(function(n){var r=g(Pt,"e",st(Er(n)));if(r.b){if(r.b.b){var t=r.a,u=r.b.a,e=g(lt,0,Ft(g(Bt,"+",u)?g(St,1,u):u)),i=Tr(t),o=C(i.a,i.b);return C(n<0?"-":"",e<0?g(lt,"0",g(At,function(n){return n.a+"."+n.b},g(At,Rt(_r),pt(C(g(wt,Er(e),"0"),o))))):p(Ct,e+1,"0",o))}return C(n<0?"-":"",t=r.a)}return""}(Er(t))),i=e.a,o=e.b,f=jt(i)+r,a=C(g(wt,1-f,"0"),p(Ct,f,"0",C(i,o))),c=jt(a),v=g(yt,1,f),b=g(n,u,p(Et,v,c,a)),s=p(Et,0,v,a),l=b?Nt(g(lt,"1",g(At,It,pt(Nt(s))))):s,d=jt(l),h="0"===l?l:0<r?A(r,jt(o))<0?p(Et,0,d-r,l)+"."+p(Et,d-r,d,l):C(i+".",p(Ct,r,"0",o)):C(l,g(wt,Er(r),"0"));return g(Lt,u,h)})(a(function(n,r){var t=pt(r);if(1===t.$)return!1;if("5"!==t.a.a)return 53<(u=gt(t.a.a))&&n||53<=u&&!n;if(""===t.a.b){return!n}var u;return!0}))),qt=g(tt,Dt(3),st),Jt=t(function(n,r,t){return{b:t,k:n,j:r}}),Ht=t(function(n,r,t){return{b:t,x:r,y:n}}),Mt=t(function(n,r,t){for(;;){if(!t.b)return r;var u=t.b,e=n,i=g(n,t.a,r);n=e,r=i,t=u}}),zt=function(n){return p(Mt,at,N,n)},Wt=u(function(n,r,t,u){if(u.b){var e=u.a,i=u.b;if(i.b){var o=i.a,f=i.b;if(f.b){var a=f.a,c=f.b;if(c.b){var v=c.b;return g(n,e,g(n,o,g(n,a,g(n,c.a,500<t?p(Mt,n,r,zt(v)):s(Wt,n,r,t+1,v)))))}return g(n,e,g(n,o,g(n,a,r)))}return g(n,e,g(n,o,r))}return g(n,e,r)}return r}),Gt=t(function(n,r,t){return s(Wt,n,r,0,t)}),Yt=a(function(t,n){return p(Gt,a(function(n,r){return g(at,t(n),r)}),N,n)}),Kt=u(function(n,r,t,u){return{bc:r,be:n,bp:t,bw:u}}),Qt=K(1/0),Ut=Y,Vt=/.^/,Xt=a(function(n,r){var t=g(lt,Vt,function(n){return g(Ut,{ao:!1,aC:!1},n)}(n));return g(At,g(tt,function(n){return n.bw},Yt(lt(""))),Or(g(Qt,t,r)))}),Zt=a(function(n,r){var t=r.k,u=r.j;return{b:n(r.b),k:n(t),j:n(u)}}),nu=a(function(n,r){var t=r.y,u=r.x;return{b:n(r.b),x:n(u),y:n(t)}}),ru=a(function(n,r){var t=r.E,u=r.g,e=r.H;return u.$?{E:t,H:e,g:Lr(g(nu,n,u.a))}:{E:t,H:e,g:Nr(g(Zt,n,u.a))}}),tu=u(function(n,r,t,u){return{$:0,a:n,b:r,c:t,d:u}}),uu=M,eu=a(function(n,r){return W(r)/W(n)}),iu=uu(g(eu,2,32)),ou=[],fu=s(tu,0,iu,ou,ou),au=d,cu=a(function(n,r){for(;;){var t=g(au,32,n),u=t.b,e=g(at,{$:0,a:t.a},r);if(!u.b)return zt(e);n=u,r=e}}),vu=a(function(n,r){for(;;){var t=uu(r/32);if(1===t)return g(au,32,n).a;n=g(cu,n,N),r=t}}),bu=z,su=function(n){return n.length},lu=a(function(n,r){if(r.c){var t=32*r.c,u=bu(g(eu,32,t-1)),e=n?zt(r.f):r.f,i=g(vu,e,r.c);return s(tu,su(r.e)+t,g(yt,5,u*iu),i,r.e)}return s(tu,su(r.e),iu,ou,r.e)}),du=l,hu=e(function(n,r,t,u,e){for(;;){if(r<0)return g(lu,!1,{f:u,c:t/32|0,e:e});var i={$:1,a:p(du,32,r,n)};n=n,r=r-32,t=t,u=g(at,i,u),e=e}}),gu=a(function(n,r){if(0<n){var t=n%32,u=p(du,t,n-t,r);return v(hu,r,n-t-32,n,N,u)}return fu}),pu=function(n){return{$:1,a:n}},$u=function(n){return{$:0,a:n}},mu=function(n){return!n.$},yu=a(function(n,r){return{$:3,a:n,b:r}}),Au=a(function(n,r){return{$:0,a:n,b:r}}),ju=a(function(n,r){return{$:1,a:n,b:r}}),xu=function(n){return{$:2,a:n}},ku=O,wu=t(function(n,r,t){for(;;){if(1<=A(n,r))return t;var u=n,e=r-1,i=g(at,r,t);n=u,r=e,t=i}}),Cu=a(function(n,r){return p(wu,n,r,N)}),Nu=a(function(n,r){return p(ku,n,g(Cu,0,Dr(r)-1),r)}),Eu=R,_u=H,Tu=a(function(n,r){return g(B,n,L(r))}),Lu=ln,Ou=a(function(n,r){return"\n\n("+_u(n+1)+") "+qr(Iu(r))}),Iu=function(n){return g(Pu,n,N)},Pu=a(function(n,r){n:for(;;)switch(n.$){case 0:var u=n.a,t=n.b,e=function(){var n=pt(u);if(1===n.$)return!1;var r=n.a,t=r.b;return Fr(r.a)&&g(Eu,Rr,t)}(),i=t,o=g(at,e?"."+u:"['"+u+"']",r);n=i,r=o;continue n;case 1:t=n.b;var f="["+_u(n.a)+"]";i=t,o=g(at,f,r);n=i,r=o;continue n;case 2:var a=n.a;if(a.b){if(a.b.b){var c=(r.b?"The Json.Decode.oneOf at json"+g(Tu,"",zt(r)):"Json.Decode.oneOf")+" failed in the following "+_u(Dr(a))+" ways:";return g(Tu,"\n\n",g(at,c,g(Nu,Ou,a)))}n=i=t=a.a,r=o=r;continue n}return"Ran into a Json.Decode.oneOf with no possibilities"+(r.b?" at json"+g(Tu,"",zt(r)):"!");default:var v=n.a,b=n.b;return(c=r.b?"Problem with the value at json"+g(Tu,"",zt(r))+":\n\n    ":"Problem with the given value:\n\n")+(qr(g(Lu,4,b))+"\n\n")+v}}),Su=dn,Bu=a(function(n,r){return dn(p(Mt,function(t){return a(function(n,r){return r.push(hn(t(n))),r})}(n),[],r))}),Fu=(nt=function(n){var r=n.b;return g(Bu,Pr,T([Su(n.a),Su(r)]))},Sn(Zr="plot"),_n[Zr]={e:Bn,r:nt,a:Fn},On(Zr)),Ru=g(tt,dt,lt(0)),Du=function(n){return{$:2,m:n}}(N),qu=t(function(n,r,t){return{E:n,H:r,g:t}}),Ju=a(function(n,r){switch(n.$){case 0:return Nr(w(r,{k:n.a}));case 1:return Nr(w(r,{j:n.a}));case 2:return Nr(w(r,{b:n.a}));case 6:return g(ru,qt,p(qu,"",!1,Lr(function(n){var r=n.g;if(r.$)return r.a;var t=r.a.b,u=t*(100/r.a.j-1);return{b:t,x:u,y:r.a.k/100*u}}(p(qu,"",!1,Nr(g(Zt,Ru,r))))))).g;default:return Nr(r)}}),Hu=a(function(n,r){switch(n.$){case 3:return Lr(w(r,{y:n.a}));case 4:return Lr(w(r,{x:n.a}));case 2:return Lr(w(r,{b:n.a}));case 5:return g(ru,qt,p(qu,"",!1,Nr(Ir(p(qu,"",!1,Lr(g(nu,Ru,r))))))).g;default:return Lr(r)}}),Mu=a(function(n,r){var t,u=w(r,(t=r.g).$?{g:g(Hu,n,t.a)}:{g:g(Ju,n,t.a)}),e=Ir(g(ru,Ru,u));return x(u,Fu(x(e.k,e.j)))}),zu=tn,Wu=un,Gu=function(n){return{$:0,a:n}},Yu=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Ku=Hn("div"),Qu=dn,Uu=a(function(n,r){return g(Wn,n,Qu(r))}),Vu=Uu("className"),Xu=a(function(n,r){return g(Ku,T([Vu(n)]),T([r]))}),Zu=t(function(n,r,t){return n(r(t))}),ne=g(Zu,Xu("row justify-content-center"),Xu("col-lg-6")),re=a(function(n,r){var t=Ir(n);return t.k*(1-t.j/100)+r*t.j/100}),te=Hn("table"),ue=Hn("tbody"),ee=Hn("td"),ie=Jn,oe=Hn("th"),fe=Hn("thead"),ae=Hn("tr"),ce=a(function(n,r){return g(Gn,function(n){return/^(on|formAction$)/i.test(n)?"data-"+n:n}(n),function(n){return/^\s*(javascript:|data:text\/html)/i.test(n)?"":n}(r))}),ve=Uu("scope"),be=a(function(n,r){function t(n){return g(ee,N,T([ie(n)]))}var u=g(fe,T([Vu("thead-light")]),T([g(ae,N,g(Yt,g(Zu,g(Zu,oe(T([ve("col")])),Hr),ie),n))])),e=(ce("border"),g(ue,N,g(Yt,function(n){return g(ae,T([Vu(function(n){if(n.b&&n.b.b&&n.b.b.b&&!n.b.b.b.b){var r=dt(n.b.b.a);return r.$?"":"color-"+function(n){return n<60?"f":n<70?"d":n<80?"c":n<90?"b":"a"}(r.a)}return""}(n))]),g(Yt,t,n))},r)));return g(te,T([Vu("table table-sm")]),T([u,e]))}),se=t(function(n,r,t){var u=n(r);return u.$?t:g(at,u.a,t)}),le=a(function(n,r){return p(Gt,se(n),N,r)}),de=function(n){for(;;){if(!n.b)return N;if(n.a.b){var r=n.a,t=r.a,u=r.b,e=g(le,Mr,o=n.b),i=g(le,Or,o);return g(at,g(at,t,i),de(g(at,u,e)))}var o;n=o=n.b}},he=a(function(n,r){var t=g(Yt,function(n){return g(Yt,qt,n)},r),u=de(t);return g(be,n,u)}),ge=a(function(n,r){var t=Ir(n);return(r-t.k*(1-t.j/100))/(t.j/100)}),pe=a(function(t,n){return p(Gt,a(function(n,r){return t(n)?g(at,n,r):r}),N,n)}),$e=a(function(n,r){return x(n,r)}),me={$:5},ye={$:6},Ae=Hn("a"),je=zn,xe=a(function(n,r){return g(je,n,function(n){return{$:0,a:n}}(r))}),ke=t(function(n,r,t){return g(Ae,T([Vu("nav-link"+(n?" active":"")),function(n){return g(xe,"click",Gu(n))}(r),function(n){return g(Uu,"href",function(n){return/^javascript:/i.test(n.replace(/\s/g,""))?"":n}(n))}("#")]),T([ie(t)]))}),we=Hn("nav"),Ce=Hn("input"),Ne=Hn("label"),Ee=Hn("span"),_e=Uu("type"),Te=Uu("value"),Le=a(function(n,r){return g(je,n,function(n){return{$:1,a:n}}(r))}),Oe=X,Ie=a(function(n,r){return p(Gt,Oe,r,n)}),Pe=V,Se=g(Ie,T(["target","value"]),Pe),Be=u(function(n,r,t,u){return g(Ee,N,T([g(Ne,N,T([ie(u)])),g(Jr,"input-group",T([g(Ce,T([Vu("numberInput form-control"),Xr(r),_e("number"),Te(t),Ur("any")]),N),n?g(Jr,"input-group-append",T([g(Ee,T([Vu("input-group-text")]),T([ie("%")]))])):g(Ku,N,N)]))]))}),Fe=Hn("button"),Re=Uu("id"),De=Pr,qe=function(n){for(;;){n=n}},Je=gn,He=Je(0),Me=$n,ze=a(function(r,n){return g(Me,function(n){return Je(r(n))},n)}),We=t(function(t,n,u){return g(Me,function(r){return g(Me,function(n){return Je(g(t,r,n))},u)},n)}),Ge=Ln,Ye=a(function(n,r){var t=r;return An(g(Me,Ge(n),t))}),Ke=t(function(n,r){return g(ze,function(){return 0},function(n){return p(Gt,We(at),Je(N),n)}(g(Yt,Ye(n),r)))}),Qe=t(function(){return Je(0)}),Ue=a(function(n,r){return g(ze,n,r)});_n.Task={b:He,c:Ke,d:Qe,e:Ue,f:rt};function Ve(n){return""===n}var Xe,Ze=On("Task"),ni=a(function(n,r){return Ze(g(ze,n,r))}),ri=J,ti=a(function(n,r){return n<1?"":p(Et,0,n,r)}),ui=D,ei=i(function(n,r,t,u,e,i){return{au:i,ax:r,aF:u,aH:t,aK:n,aL:e}}),ii=e(function(n,r,t,u,e){if(Ve(e)||g(ui,"@",e))return et;var i=g(ri,":",e);if(i.b){if(i.b.b)return et;var o=i.a,f=Ft(g(St,o+1,e));if(1===f.$)return et;var a=f;return ut(b(ei,n,g(ti,o,e),a,r,t,u))}return ut(b(ei,n,e,et,r,t,u))}),oi=u(function(n,r,t,u){if(Ve(u))return et;var e=g(ri,"/",u);if(e.b){var i=e.a;return v(ii,n,g(St,i,u),r,t,g(ti,i,u))}return v(ii,n,"/",r,t,u)}),fi=t(function(n,r,t){if(Ve(t))return et;var u=g(ri,"?",t);if(u.b){var e=u.a;return s(oi,n,ut(g(St,e+1,t)),r,g(ti,e,t))}return s(oi,n,et,r,t)}),ai=(a(function(n,r){if(Ve(r))return et;var t=g(ri,"#",r);if(t.b){var u=t.a;return p(fi,n,ut(g(St,u+1,r)),g(ti,u,r))}return p(fi,n,et,r)}),rn),ci=U,vi=Z,bi=Ar({bd:function(n){var r=n.a,t=g(ru,qt,{E:n.b,H:n.c,g:Nr({b:100,k:88,j:25})}),u=w(t,{g:g(lt,t.g,function(n){var r=g(Xt,".*\\?type=percent&grade=(.*)&percentAsstWorth=(.*)&asstPoints=(.*)",n);if(!r.$&&r.a.b&&r.a.b.b&&r.a.b.b.b&&!r.a.b.b.b.b){var t=r.a,u=t.b;return ut(Nr(p(Jt,t.a,u.a,u.b.a)))}var e=g(Xt,".*\\?type=point&numerator=(.*)&denominator=(.*)&asstPoints=(.*)",n);if(!e.$&&e.a.b&&e.a.b.b&&e.a.b.b.b&&!e.a.b.b.b.b){var i=e.a,o=i.b;return ut(Lr(p(Ht,i.a,o.a,o.b.a)))}return et}(r))}),e=Ir(g(ru,Ru,u));return x(u,Fu(x(e.k,e.j)))},bx:function(){return Du},bB:Mu,bD:function(n){return g(Ku,N,T([g(Xu,"text-center mt-2 mb-2",g(Fe,T([_e("button"),Vu("btn btn-primary"),Re("copyButton"),g(ce,"data-toggle","tooltip"),g(ce,"data-placement","bottom"),g(ce,"title",n.H?"Copied":"Copy"),g(ce,"data-clipboard-text",function(n){var r=n.g;return C(n.E,r.$?"?type=point&numerator="+r.a.y+"&denominator="+r.a.x+"&asstPoints="+r.a.b:"?type=percent&grade="+r.a.k+"&percentAsstWorth="+r.a.j+"&asstPoints="+r.a.b)}(n))]),T([ie("Copy link to this calculation")]))),ne(g(Jr,"tabcontent card container-fluid mb-3",T([g(Jr,"card-header",T([function(n){var r=n.$?x(!1,!0):x(!0,!1),t=r.a,u=r.b;return g(we,T([Vu("nav nav-tabs card-header-tabs")]),T([p(ke,t,me,"Percents"),p(ke,u,ye,"Points")]))}(n.g)])),function(u){return g(Jr,"card-body",function(){var n=u.g;if(n.$){var r=n.a;return T([g(Ee,N,T([g(Ne,N,T([ie("Current grade (points)")])),g(Jr,"input-group",T([g(Ce,T([Vu("numberInput form-control"),Xr(Qr),_e("number"),Te(r.y),Ur("any")]),N),g(Xu,"input-group-append",g(Ee,T([Vu("input-group-text")]),T([ie("/")]))),g(Ce,T([Vu("numberInput form-control"),Xr(Kr),_e("number"),Te(r.x),Ur("any"),g(ce,"aria-label","Points denominator")]),N),g(Xu,"input-group-append",g(Ee,T([Vu("input-group-text")]),T([ie("= "+qt(Ir(g(ru,Ru,u)).k)+"%")])))]))])),s(Be,!1,Wr,r.b,"Assignment total points: ")])}var t=n.a;return T([s(Be,!0,Gr,t.k,"Current grade (%): "),s(Be,!0,Yr,t.j,"Percent of grade assignment is worth (%): "),s(Be,!1,Wr,t.b,"Assignment total points (optional): ")])}())}(n)]))),ne(function(r){var n=re(r),t=g(Yt,function(n){return 10*n},g(Cu,0,11)),u=g(Yt,function(n){return n/100*Ir(r).b},t),e=g(Yt,n,t);return g(he,T(["Assignment grade (points)","Assignment grade (%)","Total grade"]),T([u,t,e]))}(g(ru,Ru,n))),ne(zr(g(ru,Ru,n)))]))}});Xe={Main:{init:bi(g(ai,function(t){return g(ai,function(r){return g(ai,function(n){return Gu(function(n,r,t){return{a:n,b:r,c:t}}(t,r,n))},g(vi,2,ci))},g(vi,1,Pe))},g(vi,0,Pe)))(0)}},n.Elm?function n(r,t){for(var u in t)u in r?"init"==u?$(6):n(r[u],t[u]):r[u]=t[u]}(n.Elm,Xe):n.Elm=Xe}(this);