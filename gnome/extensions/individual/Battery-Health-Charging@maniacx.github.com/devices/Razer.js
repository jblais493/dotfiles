'use strict';
/* Razer Laptops using package razer-cli from https://github.com/Razer-Linux/razer-laptop-control-no-dkms */
import GLib from 'gi://GLib';
import GObject from 'gi://GObject';
import * as Helper from '../lib/helper.js';

const {dmiVendor, exitCode, execCheck} = Helper;

export const  RazerSingleBattery = GObject.registerClass({
    Signals: {'threshold-applied': {param_types: [GObject.TYPE_STRING]}},
}, class RazerSingleBattery extends GObject.Object {
    constructor(settings) {
        super();
        this.name = 'Razer';
        this.type = 30;
        this.deviceNeedRootPermission = false;
        this.deviceHaveDualBattery = false;
        this.deviceHaveStartThreshold = false;
        this.deviceHaveVariableThreshold = true;
        this.deviceHaveBalancedMode = true;
        this.deviceHaveAdaptiveMode = false;
        this.deviceHaveExpressMode = false;
        this.deviceUsesModeNotValue = false;
        this.iconForFullCapMode = '100';
        this.iconForBalanceMode = '080';
        this.iconForMaxLifeMode = '060';
        this.endFullCapacityRangeMax = 100;
        this.endFullCapacityRangeMin = 100;
        this.endBalancedRangeMax = 80;
        this.endBalancedRangeMin = 65;
        this.endMaxLifeSpanRangeMax = 80;
        this.endMaxLifeSpanRangeMin = 50;
        this.incrementsStep = 5;
        this.incrementsPage = 10;

        this._settings = settings;
        this.ctlPath = null;
    }

    isAvailable() {
        if (!dmiVendor()?.includes('Razer'))
            return false;
        if (!GLib.find_program_in_path('razer-cli'))
            return false;
        return true;
    }

    async setThresholdLimit(chargingMode) {
        let output, razerWriteCommand;
        const endValue = this._settings.get_int(`current-${chargingMode}-end-threshold`);

        const razerReadCommand = ['razer-cli', 'read', 'bho'];
        [, output] = await execCheck(razerReadCommand);
        if (endValue === 100 && output?.includes('Battery health optimization is off')) {
            this.endLimitValue = endValue;
            this.emit('threshold-applied', 'success');
            return exitCode.SUCCESS;
        }
        let matchOutput = output?.match(/Battery health optimization is on with a threshold of (\d+)/);
        if (matchOutput && endValue === parseInt(matchOutput[1])) {
            this.endLimitValue = endValue;
            this.emit('threshold-applied', 'success');
            return exitCode.SUCCESS;
        }

        if (endValue === 100)
            razerWriteCommand = ['razer-cli', 'write', 'bho', 'off'];
        else
            razerWriteCommand = ['razer-cli', 'write', 'bho', 'on', `${endValue}`];

        [, output] = await execCheck(razerWriteCommand);
        if (endValue === 100 && output?.includes('Successfully turned off bho')) {
            this.endLimitValue = 100;
            this.emit('threshold-applied', 'success');
            return exitCode.SUCCESS;
        }

        matchOutput = output?.match(/Battery health optimization is on with a threshold of (\d+)/);
        if (matchOutput && endValue === parseInt(matchOutput[1])) {
            this.endLimitValue = endValue;
            this.emit('threshold-applied', 'success');
            return exitCode.SUCCESS;
        }

        this.emit('threshold-applied', 'not-updated');
        return exitCode.ERROR;
    }

    destroy() {
        // Nothing to destroy for this device
    }
});

