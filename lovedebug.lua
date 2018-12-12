function love.load()
    t = 0
    fps = 0
    debugOpen = false
    debugOpened = false
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
end

function love.draw()
    if debugOpen then
        love.graphics.print("Time: " .. t, 5, 5)
        love.graphics.print("FPS: " .. fps, 5, 25)
    end
end
