function love.load()
    t = 0
    fps = 0
    debugOpen = false
    debugOpened = false
    love.window.setFullscreen(true)

    polygon = testPolygon()
    setTotalTargetAngle(polygon)
    setStartingAngles(polygon)
end

function love.update(dt)
    t = t + dt
    fps = math.floor(1 / dt)

    if love.keyboard.isDown("f3") then
        if not debugOpened then
            debugOpen = not debugOpen
            debugOpened = true
        end
    else
        debugOpened = false
    end

    if love.keyboard.isDown("pause") then
        love.event.quit()
    end
end

function love.draw()
    if debugOpen then
        love.graphics.print("Time: " .. t, 5, 5)
        love.graphics.print("FPS: " .. fps, 5, 25)
    end

    drawPolygonTarget(polygon, 0, 0, 200)
    drawPolygonStart(polygon, 400, 0, 200)
end

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
