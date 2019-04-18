require "marble"
require "plotter"

function love.load()
    t = 0
    fps = 0
    debugOpen = false
    debugOpened = false
    love.window.setFullscreen(true)

    p = 0.5
    dp = 0.01

    plotter = Plotter:new(0, 1, 0, 1, 100, love.graphics.getWidth() - 100, 100, love.graphics.getHeight() - 100)
    marble = Marble:new(function (x) return 0.5 - 0.3 * math.sin(8 * x) end)
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
    if love.keyboard.isDown("a") then
        p = p - dp
    end
    if love.keyboard.isDown("d") then
        p = p + dp
    end
end

function love.draw()
    if debugOpen then
        love.graphics.print("Time: " .. t, 5, 5)
        love.graphics.print("FPS: " .. fps, 5, 25)
    end

    marble:plot(plotter, p)
end

function love.conf(game)
    game.console = true
end
