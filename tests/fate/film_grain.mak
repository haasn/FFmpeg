FATE_AV1_GRAIN_PROBE-$(call DEMDEC, IVF, AV1) += fate-av1-grain-probe
fate-av1-grain-probe: CMD = run ffprobe$(PROGSSUF)$(EXESUF) -show_frames \
    -export_side_data film_grain $(TARGET_SAMPLES)/av1/film_grain.ivf

FATE_SAMPLES_FFPROBE += $(FATE_AV1_GRAIN_PROBE-yes)
