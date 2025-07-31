-- Benchmarking script for incrementing a counter.
-- Run with:
-- wrk --script scripts/increment.lua --latency http://127.0.0.1:9000
request = function()
    headers = {}
    body = ''
    return wrk.format("POST", "/counters/api/v1/key1", headers, body)
end
