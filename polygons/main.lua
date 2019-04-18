require "marble"
require "plotter"

function love.load()
    t = 0
    fps = 0
    debugOpen = false
    debugOpened = false
    love.window.setFullscreen(true)

    plotter = Plotter:new(0, 1, 0, 1, 100, love.graphics.getWidth() - 100, 100, love.graphics.getHeight() - 100)
    marble = Marble:new(function (x) return 0.5 - 0.5 * math.sin(3 * x) end)
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

    if love.keyboard.isDown("escape") then
        love.event.quit()
    end
end

function love.draw()
    if debugOpen then
        love.graphics.print("Time: " .. t, 5, 5)
        love.graphics.print("FPS: " .. fps, 5, 25)
    end

    plotter:axes()
    plotter:plotFunction(marble.loss)
end

function love.conf(game)
    game.console = true
end
