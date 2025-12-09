-- Benchmarking script for decrementing a counter.
-- Run with:
-- wrk --script scripts/decrement.lua --latency http://127.0.0.1:3000
request = function()
    headers = {}
    body = ''
    return wrk.format("DELETE", "/counters/api/v1/wrknum", headers, body)
end
