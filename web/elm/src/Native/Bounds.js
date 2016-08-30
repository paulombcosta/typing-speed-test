// import Maybe, Native.Scheduler //

var _user$project$Native_Bounds = function() {

    function get(id) {
        return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
            var elem = document.getElementById(id);
            return callback(_elm_lang$core$Native_Scheduler.succeed(
                (elem == null) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(elem.getBoundingClientRect())
            ));
        });
    }

    return {
        get: get
    };

}();
