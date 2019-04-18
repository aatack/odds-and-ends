function polygonNode(targetLength, targetAngle)
    return {
        targetLength = targetLength,
        targetAngle = targetAngle
    }
end

function setTotalTargetAngle(polygon)
    local sum = 0
    for _, node in ipairs(polygon) do
        sum = sum + node.targetAngle
    end
    polygon.totalTargetAngle = sum
end

function setStartingAngles(polygon)
    for _, node in ipairs(polygon) do
        node.startingAngle = node.targetAngle / polygon.totalTargetAngle
    end
end

function testPolygon()
    return {
        polygonNode(1, 0.2),
        polygonNode(1.2, 0.2),
        polygonNode(1.5, 0.2),
        polygonNode(1.3, 0.2)
    }
end

function drawPolygonTarget(polygon, translateX, translateY, scale)
    local x = love.graphics.getWidth() / 2 + translateX
    local y = love.graphics.getHeight() / 2 + translateY
    local t = 0
    for _, node in ipairs(polygon) do
        t = t + node.targetAngle
        newX = x + node.targetLength * math.cos(t * 2 * math.pi) * scale
        newY = y + node.targetLength * math.sin(t * 2 * math.pi) * scale
        love.graphics.line(x, y, newX, newY)
        x = newX
        y = newY
    end
end

function drawPolygonStart(polygon, translateX, translateY, scale)
    local x = love.graphics.getWidth() / 2 + translateX
    local y = love.graphics.getHeight() / 2 + translateY
    local t = 0
    for _, node in ipairs(polygon) do
        t = t + node.startingAngle
        newX = x + node.targetLength * math.cos(t * 2 * math.pi) * scale
        newY = y + node.targetLength * math.sin(t * 2 * math.pi) * scale
        love.graphics.line(x, y, newX, newY)
        x = newX
        y = newY
    end
end
