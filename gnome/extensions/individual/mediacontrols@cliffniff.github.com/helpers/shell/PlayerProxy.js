import { MPRIS_PLAYER_IFACE_NAME, MPRIS_OBJECT_PATH, LoopStatus } from "../../types/enums/common.js";
import { errorLog, handleError } from "../../utils/common.js";
import { createDbusProxy } from "../../utils/shell_only.js";
import GLib from "gi://GLib";
export default class PlayerProxy {
    isPinned;
    mprisProxy;
    mprisPlayerProxy;
    propertiesProxy;
    changeListeners;
    pollSourceId;
    busName;
    isInvalid;
    constructor(busName) {
        this.busName = busName;
        this.isPinned = false;
        this.isInvalid = true;
        this.changeListeners = new Map();
    }
    async initPlayer(mprisIface, mprisPlayerIface, propertiesIface) {
        const mprisProxy = createDbusProxy(mprisIface, this.busName, MPRIS_OBJECT_PATH).catch(handleError);
        const mprisPlayerProxy = createDbusProxy(mprisPlayerIface, this.busName, MPRIS_OBJECT_PATH).catch(handleError);
        const propertiesProxy = createDbusProxy(propertiesIface, this.busName, MPRIS_OBJECT_PATH).catch(handleError);
        const proxies = await Promise.all([mprisProxy, mprisPlayerProxy, propertiesProxy]).catch(handleError);

        if (proxies == null) {
            errorLog("Failed to create proxies");
            return false;
        }

        this.mprisProxy = proxies[0];
        this.mprisPlayerProxy = proxies[1];
        this.propertiesProxy = proxies[2];

        this.propertiesProxy.connectSignal("PropertiesChanged", (proxy, senderName, [, changedProperties]) => {
            for (const [property, value] of Object.entries(changedProperties)) {
                this.callOnChangedListeners(property, value.recursiveUnpack());
            }
        });

        this.onChanged("Metadata", this.validatePlayer.bind(this));
        this.onChanged("Identity", this.validatePlayer.bind(this));
        this.onChanged("DesktopEntry", this.validatePlayer.bind(this));
        this.validatePlayer();
        this.pollTillInitialized();
        return true;
    }
    pinPlayer() {
        this.isPinned = true;
        this.callOnChangedListeners("IsPinned", this.isPinned);
    }
    unpinPlayer() {
        this.isPinned = false;
        this.callOnChangedListeners("IsPinned", this.isPinned);
    }
    isPlayerPinned() {
        return this.isPinned;
    }
    /**
     * Some players don't set the initial position and metadata immediately on startup
     */
    pollTillInitialized() {
        const timeout = 5000;
        const interval = 250;
        let count = Math.ceil(timeout / interval);

        this.pollSourceId = GLib.timeout_add(GLib.PRIORITY_DEFAULT, interval, () => {
            count--;
            const positionPromise = this.propertiesProxy.GetAsync(MPRIS_PLAYER_IFACE_NAME, "Position");
            const metadataPromise = this.propertiesProxy.GetAsync(MPRIS_PLAYER_IFACE_NAME, "Metadata");

            Promise.all([positionPromise, metadataPromise])
                .then(([positionVariant, metadataVariant]) => {
                const unpackedPosition = positionVariant[0].recursiveUnpack();
                const unpackedMetadata = metadataVariant[0].recursiveUnpack();

                if (unpackedPosition > 0 && unpackedMetadata["mpris:length"] > 0) {
                    this.mprisPlayerProxy.set_cached_property("Position", positionVariant[0]);
                    this.mprisPlayerProxy.set_cached_property("Metadata", metadataVariant[0]);
                    this.callOnChangedListeners("Metadata", unpackedMetadata);
                    GLib.source_remove(this.pollSourceId);
                }
                else if (count <= 0) {
                    GLib.source_remove(this.pollSourceId);
                }
            })
                .catch(() => {
                GLib.source_remove(this.pollSourceId);
            });

            return GLib.SOURCE_CONTINUE;
        });
    }
    validatePlayer() {
        const isValidName = this.mprisProxy.Identity || this.mprisProxy.DesktopEntry;
        const isValidMetadata = this.metadata && this.metadata["xesam:title"];
        this.isInvalid = !isValidName || !isValidMetadata;
        this.callOnChangedListeners("IsInvalid", this.isInvalid);
    }
    unpackMetadata(metadata) {
        const unpackedMetadata = {};

        for (const [key, value] of Object.entries(metadata)) {
            unpackedMetadata[key] = value.recursiveUnpack();
        }

        return unpackedMetadata;
    }
    callOnChangedListeners(property, value) {
        const listeners = this.changeListeners.get(property);

        if (listeners == null) {
            return;
        }

        for (const listener of listeners) {
            try {
                listener(value);
            }
            catch (error) {
                errorLog(`Failed to call listener for property ${property}:`, error);
            }
        }
    }
    get playbackStatus() {
        return this.mprisPlayerProxy.PlaybackStatus;
    }
    get loopStatus() {
        return this.mprisPlayerProxy.LoopStatus;
    }
    get rate() {
        return this.mprisPlayerProxy.Rate;
    }
    get shuffle() {
        return this.mprisPlayerProxy.Shuffle;
    }
    get metadata() {
        return this.unpackMetadata(this.mprisPlayerProxy.Metadata);
    }
    get volume() {
        return this.mprisPlayerProxy.Volume;
    }
    get position() {
        return this.propertiesProxy
            .GetAsync(MPRIS_PLAYER_IFACE_NAME, "Position")
            .then((result) => {
            return result[0].get_int64();
        })
            .catch(() => {
            return null;
        });
    }
    get minimumRate() {
        return this.mprisPlayerProxy.MinimumRate;
    }
    get maximumRate() {
        return this.mprisPlayerProxy.MaximumRate;
    }
    get canGoNext() {
        return this.mprisPlayerProxy.CanGoNext;
    }
    get canGoPrevious() {
        return this.mprisPlayerProxy.CanGoPrevious;
    }
    get canPlay() {
        return this.mprisPlayerProxy.CanPlay;
    }
    get canPause() {
        return this.mprisPlayerProxy.CanPause;
    }
    get canSeek() {
        return this.mprisPlayerProxy.CanSeek;
    }
    get canControl() {
        return this.mprisPlayerProxy.CanControl;
    }
    get canQuit() {
        return this.mprisProxy.CanQuit;
    }
    get canRaise() {
        return this.mprisProxy.CanRaise;
    }
    get canSetFullscreen() {
        return this.mprisProxy.CanSetFullscreen;
    }
    get desktopEntry() {
        return this.mprisProxy.DesktopEntry;
    }
    get hasTrackList() {
        return this.mprisProxy.HasTrackList;
    }
    get identity() {
        return this.mprisProxy.Identity;
    }
    get supportedMimeTypes() {
        return this.mprisProxy.SupportedMimeTypes;
    }
    get supportedUriSchemes() {
        return this.mprisProxy.SupportedUriSchemes;
    }
    set loopStatus(loopStatus) {
        this.mprisPlayerProxy.LoopStatus = loopStatus;
    }
    set rate(rate) {
        this.mprisPlayerProxy.Rate = rate;
    }
    set shuffle(shuffle) {
        this.mprisPlayerProxy.Shuffle = shuffle;
    }
    set volume(volume) {
        this.mprisPlayerProxy.Volume = volume;
    }
    set fullscreen(fullscreen) {
        this.mprisProxy.Fullscreen = fullscreen;
    }
    async next() {
        await this.mprisPlayerProxy.NextAsync().catch(handleError);
    }
    async previous() {
        await this.mprisPlayerProxy.PreviousAsync().catch(handleError);
    }
    async pause() {
        await this.mprisPlayerProxy.PauseAsync().catch(handleError);
    }
    async playPause() {
        await this.mprisPlayerProxy.PlayPauseAsync().catch(handleError);
    }
    async stop() {
        await this.mprisPlayerProxy.StopAsync().catch(handleError);
    }
    async play() {
        await this.mprisPlayerProxy.PlayAsync().catch(handleError);
    }
    async seek(offset) {
        await this.mprisPlayerProxy.SeekAsync(offset).catch(handleError);
    }
    async setPosition(trackId, position) {
        await this.mprisPlayerProxy.SetPositionAsync(trackId, position).catch(handleError);
    }
    async openUri(uri) {
        await this.mprisPlayerProxy.OpenUriAsync(uri).catch(handleError);
    }
    async raise() {
        await this.mprisProxy.RaiseAsync().catch(handleError);
    }
    async quit() {
        await this.mprisProxy.QuitAsync().catch(handleError);
    }
    toggleLoop() {
        const loopStatuses = Object.values(LoopStatus);
        const currentIndex = loopStatuses.findIndex((loop) => loop === this.loopStatus);
        const nextIndex = (currentIndex + 1 + loopStatuses.length) % loopStatuses.length;
        this.loopStatus = loopStatuses[nextIndex];
    }
    toggleShuffle() {
        this.shuffle = !this.shuffle;
    }
    onSeeked(callback) {
        const signalId = this.mprisPlayerProxy.connectSignal("Seeked", () => {
            this.position.then(callback);
        });
        return this.mprisPlayerProxy.disconnectSignal.bind(this.mprisPlayerProxy, signalId);
    }
    onChanged(property, callback) {
        const listeners = this.changeListeners.get(property);
        let id;

        if (listeners == null) {
            id = 0;
            this.changeListeners.set(property, [callback]);
        }
        else {
            id = listeners.push(callback);
        }

        return id;
    }
    removeListener(property, id) {
        const listeners = this.changeListeners.get(property);

        if (listeners == null) {
            return;
        }

        listeners.splice(id, 1);
    }
    onDestroy() {
        if (this.pollSourceId != null) {
            GLib.source_remove(this.pollSourceId);
        }
    }
}
