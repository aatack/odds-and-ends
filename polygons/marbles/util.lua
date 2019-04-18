function copyVector(v)
    local copy = {}
    for _, x in ipairs(v) do
        table.insert(copy, x)
    end
end
