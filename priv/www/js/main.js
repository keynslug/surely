$(document).ready(function () {

    String.prototype.padLeft = function (padValue) {
       return String(padValue + this).slice(-padValue.length);
    };

    var endpoint = 'ws://' + window.location.host + '/listen';
    var factory = window.WebSocket;

    if (!factory) {
        alert('WebSocket unsupported!');
        return false;
    }

    //
    // Prepare view

    var views = {};

    var formatters = {

        'ms' : function (v) {
            var s = v / 1000, m = s / 60, h = m / 60, d = h / 24, str = '';
            s %= 60; m %= 60; h %= 24;
            if (h > 24) {
                str += d.toFixed(0) + ' days, ';
            }
            return str + h.toFixed(0).padLeft('00') + ':'
                + m.toFixed(0).padLeft('00') + ':'
                + s.toFixed(3).padLeft('000000');
        },

        'b' : function (v) {
            return v.toString() + ' b';
        },

        'pc' : function (v) {
            return (v * 100).toFixed(2) + ' %';
        },

        'id' : function (v) { return v.toString(); }

    };

    var formatter = function (rep) {
        return formatters[rep || 'id'];
    };

    $('*[data-prop]').each(function (i, el) {
        var $e = $(el);
        var prop = $e.data('prop');
        var rep = $e.data('rep');
        var format = formatter(rep);
        views[prop] = {
            'update': function (v) { el.innerHTML = format(v); }
        };
    });

    var render = function (stats) {
        $.each(stats, function(name, value) {
            var v = views[name];
            if (v) { v.update(value); }
        });
    };

    //
    // Stream on

    var stream = new function() {
        var isClosed = true;
        var readyState = factory.CLOSED;
        var heartbeat;
        var delay = 80;
        var delayDefault = 80;
        var delayMax = 10000;
        var transport;

        function init() {
            isClosed = false;
            readyState = factory.CONNECTING;

            transport = new factory(endpoint);

            if (!transport) {
                stream && stream.ondisconnect();
                return false;
            }

            transport.onopen = function(){
                delay = delayDefault;
                heartbeat = setInterval(function () { stream.onheartbeat(); }, 20000);
                if (readyState != factory.OPEN) {
                    readyState = factory.OPEN;
                    stream.onopen();
                }
            };

            transport.onclose = function () {
                if (isClosed || readyState == factory.CLOSED) {
                    return;
                }

                transport = null;
                clearInterval(heartbeat);

                if (readyState == factory.CLOSING) {
                    readyState = factory.CLOSED;
                    transport = false;
                    stream.onclose();
                } else {
                    if (!isClosed && readyState == factory.OPEN) {
                        stream.ondisconnect();
                    }
                    delay *= 2;
                    if (delay > delayMax) {
                        delay = delayMax;
                    }
                    isClosed = true;
                    setTimeout(function () { init(); }, delay);
                }
            };

            transport.onerror = transport.onclose;

            transport.onmessage = function (e) {
                stream.onmessage(e);
            };

        }

        init();

        this.onopen = function(){};
        this.onmessage = function(){};
        this.ondisconnect = function(){};
        this.onclose = function(){};
        this.onheartbeat = function(){};

        this.send = function (data) {
            if (transport) {
                var ret = transport.send(data);
                return (ret === undefined) || ret;
            } else{
                return false;
            }
        };

        this.close = function () {
            readyState = factory.CLOSING;
            if (transport) {
                transport.close();
            }
        };
    };

    stream.onopen = function () { console.log('online'); }
    stream.onclose = function () { console.log('stream closed'); }
    stream.ondisconnect = function () { console.log('stream disconnected'); }

    stream.onmessage = function (m) {
        render($.parseJSON(m.data));
    }

});
