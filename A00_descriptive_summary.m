%% EAST main-results descriptive summary
% This part creates session-level and time-on-task descriptive tables 

clear; clc;

dataDir = 'data/';
outDir = 'outputs/';


%% Session-level outcomes
overview = readtable('data/EAST_Data_Overview.csv');

outcomes = {
    'KSS'
    'PVT_Standard'
    'PVT_Standard_SD'
    'PVT_novel'
    'PVT_novel_SD'
    'PVT_misses'
    'pErrall'
    'pAwareErr'
    'pCorrectHit'
    'pMisses'
    'RT'
    'RT_SelfEvaluation'
};

units = {
    'rating'
    'ms'
    'ms'
    'ms'
    'ms'
    'count'
    '%'
    '%'
    '%'
    '%'
    'ms'
    'ms'
};

conditionCodes = [1 2];
conditionLabels = ["WR" "TSD"];
nRows = numel(outcomes) * numel(conditionCodes);

descriptiveOverview = table('Size', [nRows 8], ...
    'VariableTypes', {'string','string','string','double','double','double','double','double'}, ...
    'VariableNames', {'outcome','unit','condition','n','mean','sd','sem','median'});

row = 0;
for conditionIndex = 1:numel(conditionCodes)
    conditionCode = conditionCodes(conditionIndex);
    conditionMask = overview.SleepCondition == conditionCode;

    for outcomeIndex = 1:numel(outcomes)
        row = row + 1;
        outcome = outcomes{outcomeIndex};
        values = overview.(outcome)(conditionMask);
        values = values(~isnan(values));

        % The overview CSV stores primary-task and self-evaluation RT in seconds.
        if strcmp(outcome, 'RT') || strcmp(outcome, 'RT_SelfEvaluation')
            values = values * 1000;
        end

        descriptiveOverview.outcome(row) = string(outcome);
        descriptiveOverview.unit(row) = string(units{outcomeIndex});
        descriptiveOverview.condition(row) = conditionLabels(conditionIndex);
        descriptiveOverview.n(row) = numel(values);
        descriptiveOverview.mean(row) = mean(values);
        descriptiveOverview.sd(row) = std(values, 0);
        descriptiveOverview.sem(row) = std(values, 0) / sqrt(numel(values));
        descriptiveOverview.median(row) = median(values);
    end
end

writetable(descriptiveOverview, fullfile(outDir, 'descriptive_overview.csv'));

%% Time-on-task outcomes
timeFiles = {
    'error_density', 'EAST_Data_ErrD_Qs.csv', 'ErrD'
    'unaware_error_density', 'EAST_Data_UnAwErrD_Qs.csv', 'UnAwErrD'
};

quantileLabels = {'Q1', 'Q2', 'Q3'};
nRows = size(timeFiles, 1) * numel(conditionCodes) * numel(quantileLabels);

descriptiveTimeOnTask = table('Size', [nRows 7], ...
    'VariableTypes', {'string','string','string','double','double','double','double'}, ...
    'VariableNames', {'measure','quantile','condition','n','mean','sd','sem'});

row = 0;
for fileIndex = 1:size(timeFiles, 1)
    measure = timeFiles{fileIndex, 1};
    filename = timeFiles{fileIndex, 2};
    suffix = timeFiles{fileIndex, 3};
    data = readtable(fullfile(dataDir, filename));

    for conditionIndex = 1:numel(conditionCodes)
        conditionCode = conditionCodes(conditionIndex);
        conditionMask = data.SleepCond == conditionCode;

        for quantileIndex = 1:numel(quantileLabels)
            row = row + 1;
            quantile = quantileLabels{quantileIndex};
            variable = sprintf('%s_%s', quantile, suffix);
            values = data.(variable)(conditionMask);
            values = values(~isnan(values));

            descriptiveTimeOnTask.measure(row) = string(measure);
            descriptiveTimeOnTask.quantile(row) = string(quantile);
            descriptiveTimeOnTask.condition(row) = conditionLabels(conditionIndex);
            descriptiveTimeOnTask.n(row) = numel(values);
            descriptiveTimeOnTask.mean(row) = mean(values);
            descriptiveTimeOnTask.sd(row) = std(values, 0);
            descriptiveTimeOnTask.sem(row) = std(values, 0) / sqrt(numel(values));
        end
    end
end

writetable(descriptiveTimeOnTask, fullfile(outDir, 'descriptive_time_on_task.csv'));

disp('Session-level descriptive summary');
disp(descriptiveOverview);
disp('Time-on-task descriptive summary');
disp(descriptiveTimeOnTask);

%% Replicate manuscript Figure 2
% This section recreates the six-panel result overview shown in the
% manuscript

orange = [0.90 0.29 0.06];
blue = [0.00 0.45 0.74];

figure2 = figure('Color', 'w', 'Position', [100 100 1600 950]);
layout = tiledlayout(figure2, 2, 3, 'Padding', 'compact', 'TileSpacing', 'compact');

% A: Subjective sleepiness immediately before the task
axA = nexttile(layout, 1);
kssTSD = overview.KSS(overview.SleepCondition == 2);
kssWR = overview.KSS(overview.SleepCondition == 1);
plotRaincloudPair(axA, kssTSD, kssWR, orange, blue, ...
    'Ratings before the task', 'Subjective Sleepiness (KSS)', [0 10.5], 9.7);
addPanelLabel(axA, 'A');

% B: Overall error rate
axB = nexttile(layout, 2);
errorTSD = overview.pErrall(overview.SleepCondition == 2);
errorWR = overview.pErrall(overview.SleepCondition == 1);
plotRaincloudPair(axB, errorTSD, errorWR, orange, blue, ...
    'Error (%)', 'Error Rate (%)', [-10 40], 35);
addPanelLabel(axB, 'B');

% C: Error awareness rate
axC = nexttile(layout, 3);
awarenessTSD = overview.pAwareErr(overview.SleepCondition == 2);
awarenessWR = overview.pAwareErr(overview.SleepCondition == 1);
plotRaincloudPair(axC, awarenessTSD, awarenessWR, orange, blue, ...
    'Error Awareness (%)', 'Error Awareness Rate (%)', [-50 150], 130);
addPanelLabel(axC, 'C');

% D: KSS trajectory across the deprivation night
axD = nexttile(layout, 4);
kssTime = readtable(fullfile(dataDir, 'EAST_KSS_timecourse.csv'));
kssMatrix = kssTime{:, {'KSS_11pm','KSS_1am','KSS_3am','KSS_5am'}};
kssMean = mean(kssMatrix, 1, 'omitnan');
kssN = sum(~isnan(kssMatrix), 1);
kssSEM = std(kssMatrix, 0, 1, 'omitnan') ./ sqrt(kssN);

kssMeanForFigure = runningAverageEdge(kssMean, 1, 1);
hold(axD, 'on');
plotShadedLine(axD, 1:4, kssMeanForFigure, kssSEM, blue);
xline(axD, 4.42, 'k-', 'LineWidth', 5);
text(axD, 4.25, 6.85, 'Task starts', 'HorizontalAlignment', 'right', ...
    'FontSize', 15, 'FontWeight', 'bold');
xlim(axD, [0.65 4.95]);
ylim(axD, [4 7]);
xticks(axD, 1:4);
xticklabels(axD, {'11pm','1am','3am','5am'});
xlabel(axD, 'Time');
ylabel(axD, 'KSS Ratings TSD');
formatAxes(axD);
addPanelLabel(axD, 'D');

% E: Error density across task quantiles
axE = nexttile(layout, 5);
errorQuantiles = readtable(fullfile(dataDir, 'EAST_Data_ErrD_Qs.csv'));
[errorWRMean, errorWRSEM] = quantileSummary(errorQuantiles, 1, 'ErrD');
[errorTSDMean, errorTSDSEM] = quantileSummary(errorQuantiles, 2, 'ErrD');
hold(axE, 'on');
hErrorTSD = plotShadedLine(axE, 1:3, errorTSDMean, errorTSDSEM, orange);
hErrorWR = plotShadedLine(axE, 1:3, errorWRMean, errorWRSEM, blue);
xlim(axE, [0.8 3.2]);
ylim(axE, [200 450]);
xticks(axE, 1:3);
xlabel(axE, 'Task Quantile');
ylabel(axE, 'AUC');
title(axE, 'Error Density');
addStars(axE, [2 3], 437);
formatAxes(axE);
addPanelLabel(axE, 'E');

% F: Unaware-error density across task quantiles
axF = nexttile(layout, 6);
unawareQuantiles = readtable(fullfile(dataDir, 'EAST_Data_UnAwErrD_Qs.csv'));
[unawareWRMean, unawareWRSEM] = quantileSummary(unawareQuantiles, 1, 'UnAwErrD');
[unawareTSDMean, unawareTSDSEM] = quantileSummary(unawareQuantiles, 2, 'UnAwErrD');
hold(axF, 'on');
hUnawareTSD = plotShadedLine(axF, 1:3, unawareTSDMean, unawareTSDSEM, orange);
hUnawareWR = plotShadedLine(axF, 1:3, unawareWRMean, unawareWRSEM, blue);
xlim(axF, [0.8 3.2]);
ylim(axF, [100 300]);
xticks(axF, 1:3);
xlabel(axF, 'Task Quantile');
ylabel(axF, 'AUC');
title(axF, {'Unaware'; 'Error Density'});
addStars(axF, [2 3], 289);
formatAxes(axF);
addPanelLabel(axF, 'F');

figureLegend = legend(axF, [hUnawareTSD hUnawareWR], {'TSD','WR'}, ...
    'Orientation', 'horizontal', 'Box', 'off');
figureLegend.Layout.Tile = 'south';


figurePdf = fullfile(outDir, 'Figure2_replication.pdf');
set(figure2, 'PaperPositionMode', 'auto');
print2pdf(figurePdf,figure2,300)


%% Local plotting helpers
function plotRaincloudPair(ax, tsd, wr, orange, blue, yLabel, panelTitle, yLimits, bracketY)
    hold(ax, 'on');
    rng(1234);
    groups = {tsd, wr};
    colors = {orange, blue};

    for groupIndex = 1:2
        values = groups{groupIndex};
        values = values(~isnan(values));
        x = groupIndex;
        color = colors{groupIndex};

        [density, support] = simpleKDE(values);
        density = density ./ max(density) .* 0.34;
        patch(ax, [x .* ones(size(support)), x + fliplr(density)], ...
            [support, fliplr(support)], color, ...
            'FaceAlpha', 0.95, 'EdgeColor', color, 'LineWidth', 0.6);

        jitter = -0.16 + 0.055 .* (rand(size(values)) - 0.5);
        scatter(ax, x + jitter, values, 32, color, 'filled', ...
            'MarkerEdgeColor', [0.15 0.15 0.15], 'LineWidth', 0.6);

        q = quantile(values, [0.25 0.50 0.75]);
        iqrValue = q(3) - q(1);
        lowerFence = q(1) - 1.5 .* iqrValue;
        upperFence = q(3) + 1.5 .* iqrValue;
        lowerWhisker = min(values(values >= lowerFence));
        upperWhisker = max(values(values <= upperFence));
        rectangle(ax, 'Position', [x - 0.035, q(1), 0.105, q(3) - q(1)], ...
            'FaceColor', 'w', 'EdgeColor', [0.15 0.15 0.15], 'LineWidth', 0.8);
        plot(ax, [x - 0.035 x + 0.070], [q(2) q(2)], 'k-', 'LineWidth', 1.8);
        plot(ax, [x + 0.018 x + 0.018], [lowerWhisker q(1)], 'k-', 'LineWidth', 0.8);
        plot(ax, [x + 0.018 x + 0.018], [q(3) upperWhisker], 'k-', 'LineWidth', 0.8);
    end

    plot(ax, [1.08 1.92], [bracketY bracketY], 'k-', 'LineWidth', 1.5);
    text(ax, 1.5, bracketY + 0.015 .* range(yLimits), '*', ...
        'HorizontalAlignment', 'center', 'FontSize', 24, 'FontWeight', 'bold');
    xlim(ax, [0.55 2.45]);
    ylim(ax, yLimits);
    xticks(ax, [1 2]);
    xticklabels(ax, {'TSD','WR'});
    ylabel(ax, yLabel);
    title(ax, panelTitle);
    formatAxes(ax);
end

function [density, support] = simpleKDE(values)
    values = values(:);
    n = numel(values);
    valueSD = std(values, 0);
    if valueSD == 0
        valueSD = max(abs(values(1)) .* 0.05, 0.1);
    end
    bandwidth = 1.06 .* valueSD .* n.^(-1/5);
    bandwidth = max(bandwidth, eps);
    margin = max(3 .* bandwidth, 0.05 .* range(values));
    support = linspace(min(values) - margin, max(values) + margin, 160);
    z = (support - values) ./ bandwidth;
    density = mean(exp(-0.5 .* z.^2), 1) ./ (bandwidth .* sqrt(2 .* pi));
end

function lineHandle = plotShadedLine(ax, x, meanValues, semValues, color)
    patch(ax, [x fliplr(x)], ...
        [meanValues - semValues, fliplr(meanValues + semValues)], ...
        color, 'FaceAlpha', 0.16, 'EdgeColor', color, ...
        'EdgeAlpha', 0.35, 'HandleVisibility', 'off');
    lineHandle = plot(ax, x, meanValues, '-o', 'Color', color, ...
        'MarkerFaceColor', color, 'MarkerEdgeColor', color, ...
        'LineWidth', 2.8, 'MarkerSize', 7);
end

function [meanValues, semValues] = quantileSummary(data, conditionCode, suffix)
    variables = {['Q1_' suffix], ['Q2_' suffix], ['Q3_' suffix]};
    values = data{data.SleepCond == conditionCode, variables};
    meanValues = mean(values, 1, 'omitnan');
    n = sum(~isnan(values), 1);
    semValues = std(values, 0, 1, 'omitnan') ./ sqrt(n);
end

function smoothed = runningAverageEdge(values, before, after)
    smoothed = nan(size(values));
    for index = 1:numel(values)
        firstIndex = max(1, index - before);
        lastIndex = min(numel(values), index + after);
        smoothed(index) = mean(values(firstIndex:lastIndex), 'omitnan');
    end
end

function addStars(ax, xPositions, yPosition)
    for x = xPositions
        text(ax, x, yPosition, '*', 'HorizontalAlignment', 'center', ...
            'FontSize', 25, 'FontWeight', 'bold');
    end
end

function addPanelLabel(ax, label)
    text(ax, 0.01, 1.07, label, 'Units', 'normalized', ...
        'FontSize', 24, 'FontWeight', 'normal', ...
        'HorizontalAlignment', 'left', 'VerticalAlignment', 'top');
end

function formatAxes(ax)
    set(ax, 'FontSize', 14, 'LineWidth', 0.8, 'Box', 'off', ...
        'TickDir', 'out', 'Color', 'w');
end
